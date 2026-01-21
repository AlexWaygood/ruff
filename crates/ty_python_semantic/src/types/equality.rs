use ruff_python_ast::name::Name;

use super::{Truthiness, Type};
use crate::{
    Db,
    place::PlaceAndQualifiers,
    types::{
        CallArguments, KnownClass, MemberLookupPolicy, TypeContext, TypeVarBoundOrConstraints,
    },
};

pub(super) fn could_compare_equal<'db>(db: &'db dyn Db, left: Type<'db>, right: Type<'db>) -> bool {
    evaluate_type_equality(db, left, right) != Truthiness::AlwaysFalse
}

pub(super) fn could_compare_unequal<'db>(
    db: &'db dyn Db,
    left: Type<'db>,
    right: Type<'db>,
) -> bool {
    evaluate_type_equality(db, left, right) != Truthiness::AlwaysTrue
}

fn evaluate_type_equality<'db>(db: &'db dyn Db, left: Type<'db>, right: Type<'db>) -> Truthiness {
    equality_special_case(db, left, right)
        .map(Truthiness::from)
        .unwrap_or_else(|| {
            let Ok(eq_bindings) = left.try_call_dunder(
                db,
                "__eq__",
                CallArguments::positional([right]),
                TypeContext::default(),
            ) else {
                return Truthiness::Ambiguous;
            };
            let Ok(ne_bindings) = left.try_call_dunder(
                db,
                "__ne__",
                CallArguments::positional([right]),
                TypeContext::default(),
            ) else {
                return Truthiness::Ambiguous;
            };
            let eq_truthiness = eq_bindings.return_type(db).bool(db);
            if eq_truthiness == Truthiness::Ambiguous {
                return Truthiness::Ambiguous;
            }
            let ne_truthiness = ne_bindings.return_type(db).bool(db);
            if ne_truthiness == eq_truthiness {
                Truthiness::Ambiguous
            } else {
                eq_truthiness
            }
        })
}

fn equality_special_case<'db>(db: &'db dyn Db, left: Type<'db>, right: Type<'db>) -> Option<bool> {
    match (left, right) {
        (
            Type::Never
            | Type::Dynamic(_)
            | Type::AlwaysFalsy
            | Type::AlwaysTruthy
            | Type::ProtocolInstance(_)
            | Type::DataclassTransformer(_)
            | Type::TypeGuard(_)
            | Type::TypeIs(_),
            _,
        )
        | (
            _,
            Type::Never
            | Type::Dynamic(_)
            | Type::AlwaysFalsy
            | Type::AlwaysTruthy
            | Type::ProtocolInstance(_)
            | Type::DataclassTransformer(_)
            | Type::TypeGuard(_)
            | Type::TypeIs(_),
        ) => None,

        (Type::TypeAlias(alias), other) | (other, Type::TypeAlias(alias)) => {
            equality_special_case(db, alias.value_type(db), other)
        }

        (Type::TypeVar(var), other) | (other, Type::TypeVar(var)) => {
            match var.typevar(db).bound_or_constraints(db)? {
                TypeVarBoundOrConstraints::UpperBound(bound) => {
                    equality_special_case(db, bound, other)
                }
                TypeVarBoundOrConstraints::Constraints(constraints) => {
                    equality_special_case(db, constraints.as_type(db), other)
                }
            }
        }

        (Type::NewTypeInstance(newtype), other) | (other, Type::NewTypeInstance(newtype)) => {
            equality_special_case(db, newtype.concrete_base_type(db), other)
        }

        (Type::Union(union), other) | (other, Type::Union(union)) => {
            apply_special_case_to_union_or_intersection(
                union.elements(db).iter().copied(),
                |element| equality_special_case(db, element, other),
            )
        }

        (Type::Intersection(intersection), other) | (other, Type::Intersection(intersection)) => {
            apply_special_case_to_union_or_intersection(
                intersection.positive(db).iter().copied(),
                |element| equality_special_case(db, element, other),
            )
        }

        (Type::Callable(callable), other) | (other, Type::Callable(callable)) => {
            if callable.is_function_like(db) {
                equality_special_case(db, other, KnownClass::FunctionType.to_instance(db))
            } else {
                None
            }
        }

        (Type::BooleanLiteral(b), other) | (other, Type::BooleanLiteral(b)) => {
            equality_special_case(db, Type::IntLiteral(i64::from(b)), other)
        }

        (Type::IntLiteral(l), Type::IntLiteral(r)) => Some(l == r),

        (Type::BytesLiteral(b1), Type::BytesLiteral(b2)) => Some(b1 == b2),

        (Type::EnumLiteral(l), Type::EnumLiteral(r)) => {
            let left_instance = l.enum_class_instance(db);
            let right_instance = r.enum_class_instance(db);
            let left_equality_semantics =
                KnownEqualitySemantics::for_final_instance(db, left_instance)?;
            let right_equality_semantics =
                KnownEqualitySemantics::for_final_instance(db, right_instance)?;
            if left_equality_semantics == right_equality_semantics {
                if left_equality_semantics == KnownEqualitySemantics::Object {
                    Some(l == r)
                } else {
                    Some(l.value(db) == r.value(db))
                }
            } else {
                equality_special_case(db, left_instance, right_instance)
            }
        }

        (Type::IntLiteral(int), Type::EnumLiteral(e))
        | (Type::EnumLiteral(e), Type::IntLiteral(int)) => {
            match KnownEqualitySemantics::for_final_instance(db, e.enum_class_instance(db))? {
                KnownEqualitySemantics::Int => Some(e.value(db) == Type::IntLiteral(int)),
                KnownEqualitySemantics::Bytes
                | KnownEqualitySemantics::Object
                | KnownEqualitySemantics::Str => Some(false),
            }
        }

        (Type::BytesLiteral(b), Type::EnumLiteral(e))
        | (Type::EnumLiteral(e), Type::BytesLiteral(b)) => {
            match KnownEqualitySemantics::for_final_instance(db, e.enum_class_instance(db))? {
                KnownEqualitySemantics::Bytes => Some(e.value(db) == Type::BytesLiteral(b)),
                KnownEqualitySemantics::Int
                | KnownEqualitySemantics::Object
                | KnownEqualitySemantics::Str => Some(false),
            }
        }

        (Type::StringLiteral(s), Type::EnumLiteral(e))
        | (Type::EnumLiteral(e), Type::StringLiteral(s)) => {
            match KnownEqualitySemantics::for_final_instance(db, e.enum_class_instance(db))? {
                KnownEqualitySemantics::Str => Some(e.value(db) == Type::StringLiteral(s)),
                KnownEqualitySemantics::Bytes
                | KnownEqualitySemantics::Int
                | KnownEqualitySemantics::Object => Some(false),
            }
        }

        (Type::LiteralString, Type::EnumLiteral(e))
        | (Type::EnumLiteral(e), Type::LiteralString) => {
            match KnownEqualitySemantics::for_final_instance(db, e.enum_class_instance(db))? {
                KnownEqualitySemantics::Str => None,
                KnownEqualitySemantics::Bytes
                | KnownEqualitySemantics::Int
                | KnownEqualitySemantics::Object => Some(false),
            }
        }

        (Type::TypedDict(_), Type::EnumLiteral(e)) | (Type::EnumLiteral(e), Type::TypedDict(_)) => {
            KnownEqualitySemantics::for_final_instance(db, e.enum_class_instance(db))
                .is_some()
                .then_some(false)
        }

        (Type::StringLiteral(_), other) | (other, Type::StringLiteral(_)) => {
            equality_special_case(db, Type::LiteralString, other)
        }

        (Type::LiteralString, Type::LiteralString) => None,

        (Type::DataclassDecorator(_), Type::DataclassDecorator(_)) => None,

        (Type::FunctionLiteral(l), Type::FunctionLiteral(r)) => {
            Some(l.literal(db) == r.literal(db))
        }

        (Type::FunctionLiteral(_) | Type::DataclassDecorator(_), other)
        | (other, Type::FunctionLiteral(_) | Type::DataclassDecorator(_)) => {
            // will unnecessarily return `None` in many instances if `FunctionType` is not `@final`.
            debug_assert!(
                KnownClass::FunctionType
                    .to_class_literal(db)
                    .expect_class_literal()
                    .is_final(db)
            );
            equality_special_case(db, KnownClass::FunctionType.to_instance(db), other)
        }

        (Type::BoundMethod(l), Type::BoundMethod(r)) => {
            (l.function(db).literal(db) != r.function(db).literal(db)).then_some(false)
        }

        (Type::BoundMethod(_), other) | (other, Type::BoundMethod(_)) => {
            // will unnecessarily return `None` in many instances if `MethodType` is not `@final`.
            debug_assert!(
                KnownClass::MethodType
                    .to_class_literal(db)
                    .expect_class_literal()
                    .is_final(db)
            );
            equality_special_case(db, KnownClass::MethodType.to_instance(db), other)
        }

        (Type::WrapperDescriptor(l), Type::WrapperDescriptor(r)) => (l != r).then_some(false),

        (Type::WrapperDescriptor(_), other) | (other, Type::WrapperDescriptor(_)) => {
            // will unnecessarily return `None` in many instances if `WrapperDescriptorType` is not `@final`.
            debug_assert!(
                KnownClass::WrapperDescriptorType
                    .to_class_literal(db)
                    .expect_class_literal()
                    .is_final(db)
            );
            equality_special_case(db, KnownClass::WrapperDescriptorType.to_instance(db), other)
        }

        (Type::BoundSuper(l), Type::BoundSuper(r)) => {
            (l.owner(db) != r.owner(db) && l.pivot_class(db) != r.pivot_class(db)).then_some(false)
        }

        // We could do better here but it's unclear if it's worth it.
        // There's no point delegating to `KnownClass::Super.to_instance()`
        // because `super` is not a `@final` class.
        (Type::BoundSuper(_), _) | (_, Type::BoundSuper(_)) => None,

        (Type::SpecialForm(l), Type::SpecialForm(r)) => Some(l == r),

        (Type::SpecialForm(form), other) | (other, Type::SpecialForm(form)) => {
            equality_special_case(db, form.instance_fallback(db), other)
        }

        (Type::ModuleLiteral(l), Type::ModuleLiteral(r)) => Some(l.module(db) == r.module(db)),

        // We might be able to do better here in some cases, but it's unclear if it's worth it
        (Type::ModuleLiteral(_), _) | (_, Type::ModuleLiteral(_)) => None,

        (Type::ClassLiteral(l), Type::ClassLiteral(r)) => {
            if KnownEqualitySemantics::for_final_instance(db, l.metaclass_instance_type(db))?
                == KnownEqualitySemantics::Object
            {
                Some(l == r)
            } else {
                None
            }
        }

        // we might be able to do better here after https://github.com/astral-sh/ty/issues/1859 etc. are resolved
        (Type::GenericAlias(_), _) | (_, Type::GenericAlias(_)) => None,

        // Complicated to get right in its entirety (need to recurse into inner variants);
        // unclear if the maintenance effort is worth it
        (Type::KnownBoundMethod(_), Type::KnownBoundMethod(_)) => None,

        // We could do better here too but it's unclear if it's worth it
        (Type::KnownBoundMethod(m), other) | (other, Type::KnownBoundMethod(m)) => {
            // will unnecessarily return `None` in many instances if `WrapperDescriptorType` is not `@final`.
            debug_assert!(
                m.class()
                    .to_class_literal(db)
                    .expect_class_literal()
                    .is_final(db)
            );
            equality_special_case(db, m.class().to_instance(db), other)
        }

        // We could possibly do better for `closed=True` `TypedDict`s?
        (Type::TypedDict(_), Type::TypedDict(_)) => None,

        // We might be able to do better here in some cases, but it's unclear if it's worth it
        (Type::KnownInstance(i), other) | (other, Type::KnownInstance(i)) => {
            equality_special_case(db, i.instance_fallback(db), other)
        }

        // We might be able to do better here in some cases, but it's unclear if it's worth it.
        // There's no point delegating to `KnownClass::property.to_instance()`
        // because `property` is not a `@final` class.
        (Type::PropertyInstance(_), _) | (_, Type::PropertyInstance(_)) => None,

        // We should probably do better here,
        // but we need to be careful to respect the difference between instances of `type` and generic-alias instances.
        // We also need to make sure we respect the fact that metaclasses can override `__eq__` and `__ne__`.
        (Type::SubclassOf(_), _) | (_, Type::SubclassOf(_)) => None,

        (
            Type::IntLiteral(_),
            Type::BytesLiteral(_)
            | Type::ClassLiteral(_)
            | Type::TypedDict(_)
            | Type::LiteralString,
        )
        | (
            Type::BytesLiteral(_)
            | Type::ClassLiteral(_)
            | Type::TypedDict(_)
            | Type::LiteralString,
            Type::IntLiteral(_),
        ) => Some(false),

        (
            Type::LiteralString,
            Type::BytesLiteral(_) | Type::ClassLiteral(_) | Type::TypedDict(_),
        )
        | (
            Type::BytesLiteral(_) | Type::ClassLiteral(_) | Type::TypedDict(_),
            Type::LiteralString,
        ) => Some(false),

        (Type::BytesLiteral(_), Type::ClassLiteral(_) | Type::TypedDict(_))
        | (Type::ClassLiteral(_) | Type::TypedDict(_), Type::BytesLiteral(_)) => Some(false),

        (Type::ClassLiteral(_), Type::TypedDict(_))
        | (Type::TypedDict(_), Type::ClassLiteral(_)) => Some(false),

        (Type::ClassLiteral(c), other @ (Type::NominalInstance(_) | Type::EnumLiteral(_)))
        | (other @ (Type::NominalInstance(_) | Type::EnumLiteral(_)), Type::ClassLiteral(c)) => {
            equality_special_case(db, c.metaclass_instance_type(db), other)
        }

        (Type::NominalInstance(instance), Type::IntLiteral(_))
        | (Type::IntLiteral(_), Type::NominalInstance(instance)) => {
            let class = instance.class(db);
            if class.is_final(db) {
                if KnownEqualitySemantics::for_final_instance(db, Type::NominalInstance(instance))?
                    == KnownEqualitySemantics::Int
                {
                    None
                } else {
                    Some(false)
                }
            } else {
                None
            }
        }

        (Type::NominalInstance(instance), Type::LiteralString)
        | (Type::LiteralString, Type::NominalInstance(instance)) => {
            let class = instance.class(db);
            if class.is_final(db) {
                if KnownEqualitySemantics::for_final_instance(db, Type::NominalInstance(instance))?
                    == KnownEqualitySemantics::Str
                {
                    None
                } else {
                    Some(false)
                }
            } else {
                None
            }
        }

        (Type::NominalInstance(instance), Type::BytesLiteral(_))
        | (Type::BytesLiteral(_), Type::NominalInstance(instance)) => {
            let class = instance.class(db);
            if class.is_final(db) {
                if KnownEqualitySemantics::for_final_instance(db, Type::NominalInstance(instance))?
                    == KnownEqualitySemantics::Bytes
                {
                    None
                } else {
                    Some(false)
                }
            } else {
                None
            }
        }

        (Type::NominalInstance(instance), Type::EnumLiteral(e))
        | (Type::EnumLiteral(e), Type::NominalInstance(instance)) => equality_special_case(
            db,
            Type::NominalInstance(instance),
            e.enum_class_instance(db),
        ),

        // All inhabitants of a `TypedDict` are instances of `dict` at runtime,
        // so there's no point falling back to `KnownClass::Dict.to_instance()` (`dict` is not `@final`!).
        (Type::NominalInstance(_), Type::TypedDict(_))
        | (Type::TypedDict(_), Type::NominalInstance(_)) => None,

        (Type::NominalInstance(l), Type::NominalInstance(r))
            if l == r
                && left.is_singleton(db)
                && KnownEqualitySemantics::for_final_instance(db, left).is_some() =>
        {
            Some(true)
        }

        (Type::NominalInstance(l), Type::NominalInstance(r)) => {
            let left_class = l.class(db);
            let right_class = r.class(db);
            if left_class.is_final(db) && right_class.is_final(db) {
                let left_equality_semantics = KnownEqualitySemantics::for_final_instance(db, left)?;
                let right_equality_semantics =
                    KnownEqualitySemantics::for_final_instance(db, right)?;
                if left_equality_semantics != right_equality_semantics
                    || (left_equality_semantics == KnownEqualitySemantics::Object)
                {
                    (left_class != right_class).then_some(false)
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

fn apply_special_case_to_union_or_intersection<'a, 'db>(
    elements: impl IntoIterator<Item = Type<'db>>,
    mapper: impl Fn(Type<'db>) -> Option<bool>,
) -> Option<bool> {
    let mut result = None;
    for element in elements {
        let element_result = mapper(element)?;
        match result {
            None => result = Some(element_result),
            Some(truthiness) => {
                if truthiness != element_result {
                    return None;
                }
            }
        }
    }
    result
}

fn lookup_dunder<'db>(
    db: &'db dyn Db,
    ty: Type<'db>,
    name: &'static str,
) -> PlaceAndQualifiers<'db> {
    ty.member_lookup_with_policy(
        db,
        Name::new_static(name),
        MemberLookupPolicy::MRO_NO_OBJECT_FALLBACK,
    )
}

fn lookup_dunder_eq<'db>(db: &'db dyn Db, ty: Type<'db>) -> PlaceAndQualifiers<'db> {
    lookup_dunder(db, ty, "__eq__")
}

fn lookup_dunder_ne<'db>(db: &'db dyn Db, ty: Type<'db>) -> PlaceAndQualifiers<'db> {
    lookup_dunder(db, ty, "__ne__")
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum KnownEqualitySemantics {
    Object,
    Int,
    Str,
    Bytes,
}

impl KnownEqualitySemantics {
    fn for_final_instance<'db>(db: &'db dyn Db, instance: Type<'db>) -> Option<Self> {
        let class = instance.to_meta_type(db);
        let eq = lookup_dunder_eq(db, class);
        let ne = lookup_dunder_ne(db, class);
        if eq.place.is_undefined() && ne.place.is_undefined() {
            return Some(KnownEqualitySemantics::Object);
        }
        let int_class = KnownClass::Int.to_class_literal(db);
        if eq == lookup_dunder_eq(db, int_class) && ne == lookup_dunder_ne(db, int_class) {
            return Some(KnownEqualitySemantics::Int);
        }
        let str_class = KnownClass::Str.to_class_literal(db);
        if eq == lookup_dunder_eq(db, str_class) && ne == lookup_dunder_ne(db, str_class) {
            return Some(KnownEqualitySemantics::Str);
        }
        let bytes_class = KnownClass::Bytes.to_class_literal(db);
        if eq == lookup_dunder_eq(db, bytes_class) && ne == lookup_dunder_ne(db, bytes_class) {
            return Some(KnownEqualitySemantics::Bytes);
        }
        None
    }
}
