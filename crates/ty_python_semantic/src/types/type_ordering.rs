use std::cmp::Ordering;

use salsa::plumbing::AsId;

use crate::{
    db::Db,
    semantic_index::definition::Definition,
    types::{LiteralValueTypeKind, Protocol, TypedDictType, bound_super::SuperOwnerKind},
};

use super::{
    DynamicType, TodoType, Type, TypeGuardLike, TypeGuardType, TypeIsType, class_base::ClassBase,
    subclass_of::SubclassOfInner,
};

/// Return an [`Ordering`] that describes the canonical order in which two types should appear
/// in an [`crate::types::IntersectionType`] or a [`crate::types::UnionType`] in order for them
/// to be compared for equivalence.
///
/// Two intersections are compared lexicographically. Element types in the intersection must
/// already be sorted. Two unions are never compared in this function because DNF does not permit
/// nested unions.
///
/// ## Why not just implement [`Ord`] on [`Type`]?
///
/// It would be fairly easy to slap `#[derive(PartialOrd, Ord)]` on [`Type`], and the ordering we
/// create here is not user-facing. However, it doesn't really "make sense" for `Type` to implement
/// [`Ord`] in terms of the semantics. There are many different ways in which you could plausibly
/// sort a list of types; this is only one (somewhat arbitrary, at times) possible ordering.
pub(super) fn union_or_intersection_elements_ordering<'db>(
    db: &'db dyn Db,
    left: &Type<'db>,
    right: &Type<'db>,
    ordering_purpose: OrderingPurpose,
) -> Ordering {
    if ordering_purpose.is_normalization() {
        debug_assert_eq!(
            *left,
            left.normalized(db),
            "`left` must be normalized before a meaningful ordering \
            can be established for normalization"
        );
        debug_assert_eq!(
            *right,
            right.normalized(db),
            "`right` must be normalized before a meaningful ordering \
            can be established for normalization"
        );
    }

    if left == right {
        return Ordering::Equal;
    }

    match (left, right) {
        (Type::Never, _) => Ordering::Less,
        (_, Type::Never) => Ordering::Greater,

        (Type::LiteralValue(left), Type::LiteralValue(right)) => {
            match (left.kind(), right.kind()) {
                (LiteralValueTypeKind::LiteralString, _) => Ordering::Less,
                (_, LiteralValueTypeKind::LiteralString) => Ordering::Greater,

                (LiteralValueTypeKind::Bool(left), LiteralValueTypeKind::Bool(right)) => {
                    left.cmp(&right)
                }
                (LiteralValueTypeKind::Bool(_), _) => Ordering::Less,
                (_, LiteralValueTypeKind::Bool(_)) => Ordering::Greater,

                (LiteralValueTypeKind::Int(left), LiteralValueTypeKind::Int(right)) => {
                    left.cmp(&right)
                }
                (LiteralValueTypeKind::Int(_), _) => Ordering::Less,
                (_, LiteralValueTypeKind::Int(_)) => Ordering::Greater,

                (LiteralValueTypeKind::String(left), LiteralValueTypeKind::String(right)) => {
                    match ordering_purpose {
                        OrderingPurpose::Normalization => left.cmp(&right),
                        OrderingPurpose::Determinism => left.value(db).cmp(right.value(db)),
                    }
                }
                (LiteralValueTypeKind::String(_), _) => Ordering::Less,
                (_, LiteralValueTypeKind::String(_)) => Ordering::Greater,

                (LiteralValueTypeKind::Bytes(left), LiteralValueTypeKind::Bytes(right)) => {
                    match ordering_purpose {
                        OrderingPurpose::Normalization => left.cmp(&right),
                        OrderingPurpose::Determinism => left.value(db).cmp(right.value(db)),
                    }
                }
                (LiteralValueTypeKind::Bytes(_), _) => Ordering::Less,
                (_, LiteralValueTypeKind::Bytes(_)) => Ordering::Greater,

                (LiteralValueTypeKind::Enum(left), LiteralValueTypeKind::Enum(right)) => {
                    match ordering_purpose {
                        OrderingPurpose::Normalization => left.cmp(&right),
                        OrderingPurpose::Determinism => union_or_intersection_elements_ordering(
                            db,
                            &left.enum_class_instance(db),
                            &right.enum_class_instance(db),
                            ordering_purpose,
                        )
                        .then_with(|| left.name(db).cmp(right.name(db))),
                    }
                }
            }
        }

        (Type::LiteralValue(_), _) => Ordering::Less,
        (_, Type::LiteralValue(_)) => Ordering::Greater,

        (Type::FunctionLiteral(left), Type::FunctionLiteral(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => definition_ordering(
                db,
                left.definition(db),
                right.definition(db),
                ordering_purpose,
            )
            .then_with(|| todo!("Compare the signatures as well")),
        },
        (Type::FunctionLiteral(_), _) => Ordering::Less,
        (_, Type::FunctionLiteral(_)) => Ordering::Greater,

        (Type::BoundMethod(left), Type::BoundMethod(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => union_or_intersection_elements_ordering(
                db,
                &left.self_instance(db),
                &right.self_instance(db),
                ordering_purpose,
            )
            .then_with(|| {
                union_or_intersection_elements_ordering(
                    db,
                    &Type::FunctionLiteral(left.function(db)),
                    &Type::FunctionLiteral(right.function(db)),
                    ordering_purpose,
                )
            }),
        },
        (Type::BoundMethod(_), _) => Ordering::Less,
        (_, Type::BoundMethod(_)) => Ordering::Greater,

        (Type::KnownBoundMethod(left), Type::KnownBoundMethod(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => todo!(),
        },
        (Type::KnownBoundMethod(_), _) => Ordering::Less,
        (_, Type::KnownBoundMethod(_)) => Ordering::Greater,

        (Type::WrapperDescriptor(left), Type::WrapperDescriptor(right)) => left.cmp(right),
        (Type::WrapperDescriptor(_), _) => Ordering::Less,
        (_, Type::WrapperDescriptor(_)) => Ordering::Greater,

        (Type::DataclassDecorator(left), Type::DataclassDecorator(right)) => match ordering_purpose
        {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => todo!(),
        },
        (Type::DataclassDecorator(_), _) => Ordering::Less,
        (_, Type::DataclassDecorator(_)) => Ordering::Greater,

        (Type::DataclassTransformer(left), Type::DataclassTransformer(right)) => {
            match ordering_purpose {
                OrderingPurpose::Normalization => left.cmp(right),
                OrderingPurpose::Determinism => {
                    left.flags(db).cmp(&right.flags(db)).then_with(|| {
                        let left_specs = left.field_specifiers(db);
                        let right_specs = right.field_specifiers(db);
                        left_specs.len().cmp(&right_specs.len()).then_with(|| {
                            for (l, r) in left_specs.iter().zip(right_specs) {
                                let spec_cmp = union_or_intersection_elements_ordering(
                                    db,
                                    l,
                                    r,
                                    ordering_purpose,
                                );
                                if spec_cmp != Ordering::Equal {
                                    return spec_cmp;
                                }
                            }
                            Ordering::Equal
                        })
                    })
                }
            }
        }
        (Type::DataclassTransformer(_), _) => Ordering::Less,
        (_, Type::DataclassTransformer(_)) => Ordering::Greater,

        (Type::Callable(left), Type::Callable(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => todo!(),
        },
        (Type::Callable(_), _) => Ordering::Less,
        (_, Type::Callable(_)) => Ordering::Greater,

        (Type::ModuleLiteral(left), Type::ModuleLiteral(right)) => left.cmp(right),
        (Type::ModuleLiteral(_), _) => Ordering::Less,
        (_, Type::ModuleLiteral(_)) => Ordering::Greater,

        (Type::ClassLiteral(left), Type::ClassLiteral(right)) => left.cmp(right),
        (Type::ClassLiteral(_), _) => Ordering::Less,
        (_, Type::ClassLiteral(_)) => Ordering::Greater,

        (Type::GenericAlias(left), Type::GenericAlias(right)) => left.cmp(right),
        (Type::GenericAlias(_), _) => Ordering::Less,
        (_, Type::GenericAlias(_)) => Ordering::Greater,

        (Type::SubclassOf(left), Type::SubclassOf(right)) => {
            match (left.subclass_of(), right.subclass_of()) {
                (SubclassOfInner::Class(left), SubclassOfInner::Class(right)) => left.cmp(&right),
                (SubclassOfInner::Class(_), _) => Ordering::Less,
                (_, SubclassOfInner::Class(_)) => Ordering::Greater,
                (SubclassOfInner::Dynamic(left), SubclassOfInner::Dynamic(right)) => {
                    dynamic_elements_ordering(left, right)
                }
                (SubclassOfInner::TypeVar(left), SubclassOfInner::TypeVar(right)) => {
                    left.as_id().cmp(&right.as_id())
                }
                (SubclassOfInner::TypeVar(_), _) => Ordering::Less,
                (_, SubclassOfInner::TypeVar(_)) => Ordering::Greater,
            }
        }

        (Type::SubclassOf(_), _) => Ordering::Less,
        (_, Type::SubclassOf(_)) => Ordering::Greater,

        (Type::TypeIs(left), Type::TypeIs(right)) => {
            typeis_ordering(db, *left, *right, ordering_purpose)
        }
        (Type::TypeIs(_), _) => Ordering::Less,
        (_, Type::TypeIs(_)) => Ordering::Greater,

        (Type::TypeGuard(left), Type::TypeGuard(right)) => {
            typeguard_ordering(db, *left, *right, ordering_purpose)
        }
        (Type::TypeGuard(_), _) => Ordering::Less,
        (_, Type::TypeGuard(_)) => Ordering::Greater,

        (Type::NominalInstance(left), Type::NominalInstance(right)) => {
            union_or_intersection_elements_ordering(
                db,
                &Type::from(left.class(db)),
                &Type::from(right.class(db)),
                ordering_purpose,
            )
        }
        (Type::NominalInstance(_), _) => Ordering::Less,
        (_, Type::NominalInstance(_)) => Ordering::Greater,

        (Type::ProtocolInstance(left_proto), Type::ProtocolInstance(right_proto)) => {
            match (left_proto.inner, right_proto.inner) {
                (Protocol::FromClass(left), Protocol::FromClass(right)) => {
                    union_or_intersection_elements_ordering(
                        db,
                        &Type::from(*left),
                        &Type::from(*right),
                        ordering_purpose,
                    )
                }
                (Protocol::FromClass(_), _) => Ordering::Less,
                (_, Protocol::FromClass(_)) => Ordering::Greater,

                (Protocol::Synthesized(left), Protocol::Synthesized(right)) => {
                    match ordering_purpose {
                        OrderingPurpose::Normalization => left.cmp(&right),
                        OrderingPurpose::Determinism => {
                            let left_members = left.interface().members(db);
                            let right_members = right.interface().members(db);

                            let length_cmp = left_members.len().cmp(&right_members.len());
                            if length_cmp != Ordering::Equal {
                                return length_cmp;
                            }

                            for (left, right) in left_members.zip(right_members) {
                                let member_cmp = left.ordering(db, right, ordering_purpose);
                                if member_cmp != Ordering::Equal {
                                    return member_cmp;
                                }
                            }

                            unreachable!(
                                "Two equal synthesized protocols should share the same Salsa ID"
                            )
                        }
                    }
                }
            }
        }
        (Type::ProtocolInstance(_), _) => Ordering::Less,
        (_, Type::ProtocolInstance(_)) => Ordering::Greater,

        // This is one place where we want to compare the typevar identities directly, instead of
        // falling back on `is_same_typevar_as` or `can_be_bound_for`.
        (Type::TypeVar(left), Type::TypeVar(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => {
                let left_id = left.identity(db);
                let right_id = right.identity(db);

                left_id
                    .identity
                    .name(db)
                    .cmp(right_id.identity.name(db))
                    .then_with(|| left_id.identity.kind(db).cmp(&right_id.identity.kind(db)))
                    .then_with(|| match (left_id.paramspec_attr, right_id.paramspec_attr) {
                        (Some(left), Some(right)) => left.cmp(&right),
                        (Some(_), None) => Ordering::Less,
                        (None, Some(_)) => Ordering::Greater,
                        (None, None) => Ordering::Equal,
                    })
                    .then_with(|| match (left.default_type(db), right.default_type(db)) {
                        (Some(left), Some(right)) => union_or_intersection_elements_ordering(
                            db,
                            &left,
                            &right,
                            ordering_purpose,
                        ),
                        (Some(_), None) => Ordering::Less,
                        (None, Some(_)) => Ordering::Greater,
                        (None, None) => Ordering::Equal,
                    })
                    .then_with(|| {
                        match (
                            left_id.identity.definition(db),
                            right_id.identity.definition(db),
                        ) {
                            (Some(left), Some(right)) => {
                                definition_ordering(db, left, right, ordering_purpose)
                            }
                            (Some(_), None) => Ordering::Less,
                            (None, Some(_)) => Ordering::Greater,
                            (None, None) => Ordering::Equal,
                        }
                    })
                    .then_with(|| left_id.binding_context.cmp(&right_id.binding_context))
            }
        },
        (Type::TypeVar(_), _) => Ordering::Less,
        (_, Type::TypeVar(_)) => Ordering::Greater,

        (Type::AlwaysTruthy, _) => Ordering::Less,
        (_, Type::AlwaysTruthy) => Ordering::Greater,

        (Type::AlwaysFalsy, _) => Ordering::Less,
        (_, Type::AlwaysFalsy) => Ordering::Greater,

        (Type::BoundSuper(left), Type::BoundSuper(right)) => {
            (match (left.pivot_class(db), right.pivot_class(db)) {
                (ClassBase::Class(left), ClassBase::Class(right)) => {
                    union_or_intersection_elements_ordering(
                        db,
                        &Type::from(left),
                        &Type::from(right),
                        ordering_purpose,
                    )
                }
                (ClassBase::Class(_), _) => Ordering::Less,
                (_, ClassBase::Class(_)) => Ordering::Greater,

                (ClassBase::Protocol, _) => Ordering::Less,
                (_, ClassBase::Protocol) => Ordering::Greater,

                (ClassBase::Generic, _) => Ordering::Less,
                (_, ClassBase::Generic) => Ordering::Greater,

                (ClassBase::TypedDict, _) => Ordering::Less,
                (_, ClassBase::TypedDict) => Ordering::Greater,

                (ClassBase::Dynamic(left), ClassBase::Dynamic(right)) => {
                    dynamic_elements_ordering(left, right)
                }
            })
            .then_with(|| match (left.owner(db), right.owner(db)) {
                (SuperOwnerKind::Class(left), SuperOwnerKind::Class(right)) => {
                    union_or_intersection_elements_ordering(
                        db,
                        &Type::from(left),
                        &Type::from(right),
                        ordering_purpose,
                    )
                }
                (SuperOwnerKind::Class(_), _) => Ordering::Less,
                (_, SuperOwnerKind::Class(_)) => Ordering::Greater,
                (SuperOwnerKind::Instance(left), SuperOwnerKind::Instance(right)) => {
                    union_or_intersection_elements_ordering(
                        db,
                        &Type::from(left.class(db)),
                        &Type::from(right.class(db)),
                        ordering_purpose,
                    )
                }
                (SuperOwnerKind::Instance(_), _) => Ordering::Less,
                (_, SuperOwnerKind::Instance(_)) => Ordering::Greater,
                (
                    SuperOwnerKind::InstanceTypeVar(left, _),
                    SuperOwnerKind::InstanceTypeVar(right, _),
                ) => union_or_intersection_elements_ordering(
                    db,
                    &Type::TypeVar(left),
                    &Type::TypeVar(right),
                    ordering_purpose,
                ),
                (SuperOwnerKind::InstanceTypeVar(..), _) => Ordering::Less,
                (_, SuperOwnerKind::InstanceTypeVar(..)) => Ordering::Greater,
                (SuperOwnerKind::ClassTypeVar(left, _), SuperOwnerKind::ClassTypeVar(right, _)) => {
                    union_or_intersection_elements_ordering(
                        db,
                        &Type::TypeVar(left),
                        &Type::TypeVar(right),
                        ordering_purpose,
                    )
                }
                (SuperOwnerKind::ClassTypeVar(..), _) => Ordering::Less,
                (_, SuperOwnerKind::ClassTypeVar(..)) => Ordering::Greater,
                (SuperOwnerKind::Dynamic(left), SuperOwnerKind::Dynamic(right)) => {
                    dynamic_elements_ordering(left, right)
                }
            })
        }
        (Type::BoundSuper(_), _) => Ordering::Less,
        (_, Type::BoundSuper(_)) => Ordering::Greater,

        (Type::SpecialForm(left), Type::SpecialForm(right)) => left.cmp(right),
        (Type::SpecialForm(_), _) => Ordering::Less,
        (_, Type::SpecialForm(_)) => Ordering::Greater,

        (Type::KnownInstance(left), Type::KnownInstance(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => todo!(),
        },
        (Type::KnownInstance(_), _) => Ordering::Less,
        (_, Type::KnownInstance(_)) => Ordering::Greater,

        (Type::PropertyInstance(left), Type::PropertyInstance(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => match (left.getter(db), right.getter(db)) {
                (Some(left), Some(right)) => {
                    union_or_intersection_elements_ordering(db, &left, &right, ordering_purpose)
                }
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (None, None) => match (left.setter(db), right.setter(db)) {
                    (Some(left), Some(right)) => {
                        union_or_intersection_elements_ordering(db, &left, &right, ordering_purpose)
                    }
                    (Some(_), _) => Ordering::Less,
                    (_, Some(_)) => Ordering::Greater,
                    (None, None) => {
                        unreachable!("Two equal property instances should share the same Salsa ID")
                    }
                },
            },
        },
        (Type::PropertyInstance(_), _) => Ordering::Less,
        (_, Type::PropertyInstance(_)) => Ordering::Greater,

        (Type::Dynamic(left), Type::Dynamic(right)) => dynamic_elements_ordering(*left, *right),
        (Type::Dynamic(_), _) => Ordering::Less,
        (_, Type::Dynamic(_)) => Ordering::Greater,

        (Type::TypeAlias(left), Type::TypeAlias(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => match (left.definition(db), right.definition(db)) {
                (Some(left), Some(right)) => definition_ordering(db, left, right, ordering_purpose),
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (None, None) => union_or_intersection_elements_ordering(
                    db,
                    &left.value_type(db),
                    &right.value_type(db),
                    ordering_purpose,
                ),
            },
        },
        (Type::TypeAlias(_), _) => Ordering::Less,
        (_, Type::TypeAlias(_)) => Ordering::Greater,

        (Type::TypedDict(left), Type::TypedDict(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => match (left, right) {
                (TypedDictType::Class(left), TypedDictType::Class(right)) => {
                    union_or_intersection_elements_ordering(
                        db,
                        &Type::from(*left),
                        &Type::from(*right),
                        ordering_purpose,
                    )
                }
                (TypedDictType::Class(_), TypedDictType::Synthesized(_)) => Ordering::Less,
                (TypedDictType::Synthesized(_), TypedDictType::Class(_)) => Ordering::Greater,
                (TypedDictType::Synthesized(left), TypedDictType::Synthesized(right)) => todo!(),
            },
        },
        (Type::TypedDict(_), _) => Ordering::Less,
        (_, Type::TypedDict(_)) => Ordering::Greater,

        (Type::NewTypeInstance(left), Type::NewTypeInstance(right)) => match ordering_purpose {
            OrderingPurpose::Normalization => left.cmp(right),
            OrderingPurpose::Determinism => definition_ordering(
                db,
                left.definition(db),
                right.definition(db),
                ordering_purpose,
            ),
        },
        (Type::NewTypeInstance(_), _) => Ordering::Less,
        (_, Type::NewTypeInstance(_)) => Ordering::Greater,

        (Type::Union(_), _) | (_, Type::Union(_)) if ordering_purpose.is_normalization() => {
            unreachable!("Our type representation does not permit nested unions")
        }

        (Type::Union(left), Type::Union(right)) => {
            let recursive_definition_cmp = left
                .recursively_defined(db)
                .cmp(&right.recursively_defined(db));
            if recursive_definition_cmp != Ordering::Equal {
                return recursive_definition_cmp;
            }

            let left_elements = left.elements(db);
            let right_elements = right.elements(db);

            let length_cmp = left_elements.len().cmp(&right_elements.len());
            if length_cmp != Ordering::Equal {
                return length_cmp;
            }

            for (left, right) in left_elements.iter().zip(right_elements) {
                let ordering =
                    union_or_intersection_elements_ordering(db, left, right, ordering_purpose);
                if ordering != Ordering::Equal {
                    return ordering;
                }
            }

            unreachable!("Two equal unions should share the same Salsa ID")
        }
        (Type::Union(_), _) => Ordering::Less,
        (_, Type::Union(_)) => Ordering::Greater,

        (Type::Intersection(left), Type::Intersection(right)) => {
            // Lexicographically compare the elements of the two unequal intersections.
            let left_positive = left.positive(db);
            let right_positive = right.positive(db);

            let pos_length_cmp = left_positive.len().cmp(&right_positive.len());
            if pos_length_cmp != Ordering::Equal {
                return pos_length_cmp;
            }

            let left_negative = left.negative(db);
            let right_negative = right.negative(db);

            let neg_length_cmp = left_negative.len().cmp(&right_negative.len());
            if neg_length_cmp != Ordering::Equal {
                return neg_length_cmp;
            }

            for (left, right) in left_positive.iter().zip(right_positive) {
                let ordering =
                    union_or_intersection_elements_ordering(db, left, right, ordering_purpose);
                if ordering != Ordering::Equal {
                    return ordering;
                }
            }

            for (left, right) in left_negative.iter().zip(right_negative) {
                let ordering =
                    union_or_intersection_elements_ordering(db, left, right, ordering_purpose);
                if ordering != Ordering::Equal {
                    return ordering;
                }
            }

            unreachable!("Two equal intersections should share the same Salsa ID")
        }
    }
}

/// Determine a canonical order for two instances of [`DynamicType`].
fn dynamic_elements_ordering(left: DynamicType, right: DynamicType) -> Ordering {
    match (left, right) {
        (DynamicType::Any, _) => Ordering::Less,
        (_, DynamicType::Any) => Ordering::Greater,

        (DynamicType::Unknown, _) => Ordering::Less,
        (_, DynamicType::Unknown) => Ordering::Greater,

        (DynamicType::UnknownGeneric(left), DynamicType::UnknownGeneric(right)) => left.cmp(&right),
        (DynamicType::UnknownGeneric(_), _) => Ordering::Less,
        (_, DynamicType::UnknownGeneric(_)) => Ordering::Greater,

        (DynamicType::UnspecializedTypeVar, _) => Ordering::Less,
        (_, DynamicType::UnspecializedTypeVar) => Ordering::Greater,

        #[cfg(debug_assertions)]
        (DynamicType::Todo(TodoType(left)), DynamicType::Todo(TodoType(right))) => left.cmp(right),

        #[cfg(not(debug_assertions))]
        (DynamicType::Todo(TodoType), DynamicType::Todo(TodoType)) => Ordering::Equal,

        (DynamicType::TodoUnpack, _) => Ordering::Less,
        (_, DynamicType::TodoUnpack) => Ordering::Greater,

        (DynamicType::TodoStarredExpression, _) => Ordering::Less,
        (_, DynamicType::TodoStarredExpression) => Ordering::Greater,

        (DynamicType::TodoTypeVarTuple, _) => Ordering::Less,
        (_, DynamicType::TodoTypeVarTuple) => Ordering::Greater,

        (DynamicType::Divergent(left), DynamicType::Divergent(right)) => left.cmp(&right),
        (DynamicType::Divergent(_), _) => Ordering::Less,
        (_, DynamicType::Divergent(_)) => Ordering::Greater,
    }
}

/// Generic helper for ordering type guard-like types.
///
/// The following criteria are considered, in order:
/// * Boundness: Unbound precedes bound
/// * Symbol name: String comparison
/// * Guarded type: [`union_or_intersection_elements_ordering`]
fn guard_like_ordering<'db, T: TypeGuardLike<'db>>(
    db: &'db dyn Db,
    left: T,
    right: T,
    ordering_purpose: OrderingPurpose,
) -> Ordering {
    let (left_ty, right_ty) = (left.return_type(db), right.return_type(db));

    match (left.place_info(db), right.place_info(db)) {
        (None, Some(_)) => Ordering::Less,
        (Some(_), None) => Ordering::Greater,

        (None, None) => {
            union_or_intersection_elements_ordering(db, &left_ty, &right_ty, ordering_purpose)
        }

        (Some(_), Some(_)) => match left.place_name(db).cmp(&right.place_name(db)) {
            Ordering::Equal => {
                union_or_intersection_elements_ordering(db, &left_ty, &right_ty, ordering_purpose)
            }
            ordering => ordering,
        },
    }
}

/// Determine a canonical order for two instances of [`TypeIsType`].
fn typeis_ordering(
    db: &dyn Db,
    left: TypeIsType,
    right: TypeIsType,
    ordering_purpose: OrderingPurpose,
) -> Ordering {
    guard_like_ordering(db, left, right, ordering_purpose)
}

/// Determine a canonical order for two instances of [`TypeGuardType`].
fn typeguard_ordering(
    db: &dyn Db,
    left: TypeGuardType,
    right: TypeGuardType,
    ordering_purpose: OrderingPurpose,
) -> Ordering {
    guard_like_ordering(db, left, right, ordering_purpose)
}

fn definition_ordering(
    db: &dyn Db,
    left: Definition,
    right: Definition,
    ordering_purpose: OrderingPurpose,
) -> Ordering {
    match ordering_purpose {
        OrderingPurpose::Normalization => left.cmp(&right),
        OrderingPurpose::Determinism => left
            .file(db)
            .path(db)
            .as_str()
            .cmp(right.file(db).path(db).as_str())
            .then_with(|| left.scope(db).cmp(&right.scope(db)))
            .then_with(|| left.place(db).cmp(&right.place(db))),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum OrderingPurpose {
    Normalization,
    Determinism,
}

impl OrderingPurpose {
    const fn is_normalization(self) -> bool {
        matches!(self, OrderingPurpose::Normalization)
    }
}
