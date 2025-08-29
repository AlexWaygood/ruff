use crate::{
    Db, FxIndexSet,
    place::{Boundness, Place, PlaceAndQualifiers},
    types::{
        Bindings, CallArguments, CallDunderError, KnownClass, MemberLookupPolicy,
        PropertyInstanceType, StringLiteralType, Type, TypeQualifiers,
        context::InferContext,
        diagnostic::{
            INVALID_ASSIGNMENT, report_attempted_write_to_read_only_property,
            report_invalid_attribute_assignment, report_possibly_unbound_attribute,
        },
    },
};
use ruff_python_ast as ast;

/// Make sure that the attribute assignment `obj.attribute = value` is valid.
///
/// `attribute` is the name of the attribute being assigned, and `value_ty` is the type of the right-hand side of
/// the assignment.
pub(crate) fn validate_attribute_assignment<'db>(
    db: &'db dyn Db,
    object_assigned_to: Type<'db>,
    attribute: &str,
    value_ty: Type<'db>,
) -> Result<(), AttributeAssignmentError<'db>> {
    let ensure_assignable_to = |attr_ty| -> Result<(), AttributeAssignmentErrorKind> {
        if value_ty.is_assignable_to(db, attr_ty) {
            Ok(())
        } else {
            Err(AttributeAssignmentErrorKind::TypeMismatch(attr_ty))
        }
    };

    let mut results = AttributeAssignmentError::default();

    match object_assigned_to {
        Type::Union(union) => {
            if union.elements(db).iter().all(|elem| {
                let res = validate_attribute_assignment(db, *elem, attribute, value_ty);
                match res {
                    Ok(()) => true,
                    Err(errors) if errors.is_possibly_unbound() => {
                        results.insert(AttributeAssignmentErrorKind::PossiblyUnbound);
                        true
                    }
                    _ => false,
                }
            }) {
                results.and(Ok(()))
            } else {
                results.and(Err(AttributeAssignmentErrorKind::TypeMismatch(
                    object_assigned_to,
                )))
            }
        }

        Type::Intersection(intersection) => {
            // TODO: Handle negative intersection elements
            if intersection.positive(db).iter().any(|elem| {
                let res = validate_attribute_assignment(db, *elem, attribute, value_ty);
                match res {
                    Ok(()) => true,
                    Err(errors) if errors.is_possibly_unbound() => {
                        results.insert(AttributeAssignmentErrorKind::PossiblyUnbound);
                        true
                    }
                    _ => false,
                }
            }) {
                results.and(Ok(()))
            } else {
                results.and(Err(AttributeAssignmentErrorKind::TypeMismatch(
                    object_assigned_to,
                )))
            }
        }

        Type::TypeAlias(alias) => {
            validate_attribute_assignment(db, object_assigned_to, attribute, alias.value_type(db))
        }

        // Super instances do not allow attribute assignment
        Type::NominalInstance(instance) if instance.class(db).is_known(db, KnownClass::Super) => {
            results.and(Err(AttributeAssignmentErrorKind::CannotAssign))
        }
        Type::BoundSuper(_) => results.and(Err(AttributeAssignmentErrorKind::CannotAssign)),

        Type::Dynamic(..) | Type::Never => results.and(Ok(())),

        Type::NominalInstance(..)
        | Type::ProtocolInstance(_)
        | Type::BooleanLiteral(..)
        | Type::IntLiteral(..)
        | Type::StringLiteral(..)
        | Type::BytesLiteral(..)
        | Type::EnumLiteral(_)
        | Type::LiteralString
        | Type::SpecialForm(..)
        | Type::KnownInstance(..)
        | Type::PropertyInstance(..)
        | Type::FunctionLiteral(..)
        | Type::Callable(..)
        | Type::BoundMethod(_)
        | Type::MethodWrapper(_)
        | Type::WrapperDescriptor(_)
        | Type::DataclassDecorator(_)
        | Type::DataclassTransformer(_)
        | Type::NonInferableTypeVar(_)
        | Type::TypeVar(..)
        | Type::AlwaysTruthy
        | Type::AlwaysFalsy
        | Type::TypeIs(_)
        | Type::TypedDict(_) => {
            if let Type::ProtocolInstance(protocol) = object_assigned_to {
                if let Some(member) = protocol.interface(db).member_by_name(db, attribute) {
                    if let Err(err) = member.instance_set_type() {
                        return results.and(Err(err));
                    }
                }
            }

            // First, try to call the `__setattr__` dunder method. If this is present/defined, overrides
            // assigning the attributed by the normal mechanism.
            let setattr_dunder_call_result = object_assigned_to.try_call_dunder_with_policy(
                db,
                "__setattr__",
                &mut CallArguments::positional([
                    Type::StringLiteral(StringLiteralType::new(db, Box::from(attribute))),
                    value_ty,
                ]),
                MemberLookupPolicy::MRO_NO_OBJECT_FALLBACK,
            );

            let check_setattr_return_type = |result: Bindings<'db>| match result.return_type(db) {
                Type::Never => {
                    let is_setattr_synthesized = match object_assigned_to.class_member_with_policy(
                        db,
                        "__setattr__".into(),
                        MemberLookupPolicy::MRO_NO_OBJECT_FALLBACK,
                    ) {
                        PlaceAndQualifiers {
                            place: Place::Type(attr_ty, _),
                            qualifiers: _,
                        } => attr_ty.is_callable_type(),
                        _ => false,
                    };

                    let member_exists =
                        !object_assigned_to.member(db, attribute).place.is_unbound();

                    Err(if !member_exists {
                        AttributeAssignmentErrorKind::CannotAssignToUnresolved
                    } else if is_setattr_synthesized {
                        AttributeAssignmentErrorKind::ReadOnlyProperty(None)
                    } else {
                        AttributeAssignmentErrorKind::SetAttrReturnsNeverOrNoReturn
                    })
                }
                _ => Ok(()),
            };

            match setattr_dunder_call_result {
                Ok(bindings) => results.and(check_setattr_return_type(bindings)),
                Err(CallDunderError::PossiblyUnbound(bindings)) => {
                    results.and(check_setattr_return_type(*bindings))
                }
                Err(CallDunderError::CallError(..)) => {
                    results.and(Err(AttributeAssignmentErrorKind::FailToSetAttr))
                }
                Err(CallDunderError::MethodNotAvailable) => {
                    match object_assigned_to.class_member(db, attribute.into()) {
                        meta_attr @ PlaceAndQualifiers { .. } if meta_attr.is_class_var() => {
                            results.and(Err(AttributeAssignmentErrorKind::CannotAssignToClassVar))
                        }
                        PlaceAndQualifiers {
                            place: Place::Type(meta_attr_ty, meta_attr_boundness),
                            qualifiers,
                        } => {
                            if qualifiers.contains(TypeQualifiers::FINAL) {
                                return results
                                    .and(Err(AttributeAssignmentErrorKind::CannotAssignToFinal));
                            }

                            // Check if it is assignable to the meta attribute type.
                            if let Place::Type(meta_dunder_set, _) =
                                meta_attr_ty.class_member(db, "__set__".into()).place
                            {
                                let dunder_set_result = meta_dunder_set.try_call(
                                    db,
                                    &CallArguments::positional([
                                        meta_attr_ty,
                                        object_assigned_to,
                                        value_ty,
                                    ]),
                                );

                                if let Err(dunder_set_error) = dunder_set_result {
                                    results.insert(
                                        if let Some(property) = dunder_set_error
                                            .as_attempt_to_set_property_with_no_setter()
                                        {
                                            AttributeAssignmentErrorKind::ReadOnlyProperty(Some(
                                                property,
                                            ))
                                        } else {
                                            AttributeAssignmentErrorKind::FailToSet
                                        },
                                    );
                                }
                            } else {
                                results.insert_if_error(ensure_assignable_to(meta_attr_ty));
                            }

                            // Check if it is assignable to the instance attribute type.
                            if meta_attr_boundness == Boundness::PossiblyUnbound {
                                let (assignable, boundness) =
                                    if let Place::Type(instance_attr_ty, instance_attr_boundness) =
                                        object_assigned_to.instance_member(db, attribute).place
                                    {
                                        (
                                            ensure_assignable_to(instance_attr_ty),
                                            instance_attr_boundness,
                                        )
                                    } else {
                                        (Ok(()), Boundness::PossiblyUnbound)
                                    };

                                results.insert_if_error(assignable);

                                if boundness == Boundness::PossiblyUnbound {
                                    results.insert(AttributeAssignmentErrorKind::PossiblyUnbound);
                                }
                            }

                            results.and(Ok(()))
                        }

                        PlaceAndQualifiers {
                            place: Place::Unbound,
                            ..
                        } => {
                            if let PlaceAndQualifiers {
                                place: Place::Type(instance_attr_ty, instance_attr_boundness),
                                qualifiers,
                            } = object_assigned_to.instance_member(db, attribute)
                            {
                                if qualifiers.contains(TypeQualifiers::FINAL) {
                                    return results.and(Err(
                                        AttributeAssignmentErrorKind::CannotAssignToFinal,
                                    ));
                                }

                                if instance_attr_boundness == Boundness::PossiblyUnbound {
                                    results.insert(AttributeAssignmentErrorKind::PossiblyUnbound);
                                }
                                results.and(ensure_assignable_to(instance_attr_ty))
                            } else {
                                results.and(Err(AttributeAssignmentErrorKind::Unresolved))
                            }
                        }
                    }
                }
            }
        }

        Type::ClassLiteral(..) | Type::GenericAlias(..) | Type::SubclassOf(..) => {
            match object_assigned_to.class_member(db, attribute.into()) {
                PlaceAndQualifiers {
                    place: Place::Type(meta_attr_ty, meta_attr_boundness),
                    qualifiers,
                } => {
                    if qualifiers.contains(TypeQualifiers::FINAL) {
                        return results.and(Err(AttributeAssignmentErrorKind::CannotAssignToFinal));
                    }

                    // Check if it is assignable to the meta attribute type.
                    if let Place::Type(meta_dunder_set, _) =
                        meta_attr_ty.class_member(db, "__set__".into()).place
                    {
                        let dunder_set_result = meta_dunder_set.try_call(
                            db,
                            &CallArguments::positional([
                                meta_attr_ty,
                                object_assigned_to,
                                value_ty,
                            ]),
                        );

                        if let Err(dunder_set_error) = dunder_set_result {
                            results.insert(
                                if let Some(property) =
                                    dunder_set_error.as_attempt_to_set_property_with_no_setter()
                                {
                                    AttributeAssignmentErrorKind::ReadOnlyProperty(Some(property))
                                } else {
                                    AttributeAssignmentErrorKind::FailToSet
                                },
                            );
                        }
                    } else {
                        results.insert_if_error(ensure_assignable_to(meta_attr_ty));
                    }

                    // Check if it is assignable to the class attribute type.
                    if meta_attr_boundness == Boundness::PossiblyUnbound {
                        let (assignable, boundness) =
                            if let Place::Type(class_attr_ty, class_attr_boundness) =
                                object_assigned_to
                                    .find_name_in_mro(db, attribute)
                                    .expect("called on Type::ClassLiteral or Type::SubclassOf")
                                    .place
                            {
                                (ensure_assignable_to(class_attr_ty), class_attr_boundness)
                            } else {
                                (Ok(()), Boundness::PossiblyUnbound)
                            };

                        if boundness == Boundness::PossiblyUnbound {
                            results.insert(AttributeAssignmentErrorKind::PossiblyUnbound);
                        }

                        results.insert_if_error(assignable);
                    }

                    results.and(Ok(()))
                }
                PlaceAndQualifiers {
                    place: Place::Unbound,
                    ..
                } => {
                    if let PlaceAndQualifiers {
                        place: Place::Type(class_attr_ty, class_attr_boundness),
                        qualifiers,
                    } = object_assigned_to
                        .find_name_in_mro(db, attribute)
                        .expect("called on Type::ClassLiteral or Type::SubclassOf")
                    {
                        if qualifiers.contains(TypeQualifiers::FINAL) {
                            return results
                                .and(Err(AttributeAssignmentErrorKind::CannotAssignToFinal));
                        }

                        if class_attr_boundness == Boundness::PossiblyUnbound {
                            results.insert(AttributeAssignmentErrorKind::PossiblyUnbound);
                        }
                        results.and(ensure_assignable_to(class_attr_ty))
                    } else {
                        let attribute_is_bound_on_instance =
                            object_assigned_to.to_instance(db).is_some_and(|instance| {
                                !instance.instance_member(db, attribute).place.is_unbound()
                            });

                        // Attribute is declared or bound on instance. Forbid access from the class object
                        if attribute_is_bound_on_instance {
                            results.and(Err(
                                AttributeAssignmentErrorKind::CannotAssignToInstanceAttr,
                            ))
                        } else {
                            results.and(Err(AttributeAssignmentErrorKind::Unresolved))
                        }
                    }
                }
            }
        }

        Type::ModuleLiteral(module) => {
            if let Place::Type(attr_ty, _) = module.static_member(db, attribute).place {
                if value_ty.is_assignable_to(db, attr_ty) {
                    results.and(Ok(()))
                } else {
                    results.and(Err(AttributeAssignmentErrorKind::TypeMismatch(attr_ty)))
                }
            } else {
                results.and(Err(AttributeAssignmentErrorKind::Unresolved))
            }
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct AttributeAssignmentError<'db>(FxIndexSet<AttributeAssignmentErrorKind<'db>>);

impl<'db> IntoIterator for AttributeAssignmentError<'db> {
    type Item = AttributeAssignmentErrorKind<'db>;
    type IntoIter = ordermap::set::IntoIter<AttributeAssignmentErrorKind<'db>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'db> AttributeAssignmentError<'db> {
    pub(crate) fn is_possibly_unbound(&self) -> bool {
        self.0
            .iter()
            .any(AttributeAssignmentErrorKind::is_possibly_unbound)
    }

    fn insert(&mut self, result: AttributeAssignmentErrorKind<'db>) {
        self.0.insert(result);
    }

    fn insert_if_error(&mut self, result: Result<(), AttributeAssignmentErrorKind<'db>>) {
        if let Err(error) = result {
            self.insert(error);
        }
    }

    fn and<T>(mut self, result: Result<T, AttributeAssignmentErrorKind<'db>>) -> Result<T, Self> {
        match result {
            Ok(value) => {
                if self.0.is_empty() {
                    Ok(value)
                } else {
                    Err(self)
                }
            }
            Err(error) => {
                self.0.insert(error);
                Err(self)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum AttributeAssignmentErrorKind<'db> {
    PossiblyUnbound,
    TypeMismatch(Type<'db>),
    CannotAssign,
    CannotAssignToClassVar,
    CannotAssignToInstanceAttr,
    CannotAssignToFinal,
    CannotAssignToUnresolved,
    ReadOnlyProperty(Option<PropertyInstanceType<'db>>),
    FailToSet,
    FailToSetAttr,
    SetAttrReturnsNeverOrNoReturn,
    Unresolved,
}

impl<'db> AttributeAssignmentErrorKind<'db> {
    pub(crate) const fn is_possibly_unbound(&self) -> bool {
        matches!(self, Self::PossiblyUnbound)
    }

    pub(crate) fn into_diagnostic(
        self,
        context: &InferContext<'db, '_>,
        target: &ast::ExprAttribute,
        attribute: &str,
        object_assigned_to: Type<'db>,
        assignment_value: Type<'db>,
    ) {
        let Some(builder) = context.report_lint(&INVALID_ASSIGNMENT, target) else {
            return;
        };

        let db = context.db();

        match self {
            AttributeAssignmentErrorKind::PossiblyUnbound => {
                report_possibly_unbound_attribute(db, builder, attribute, object_assigned_to);
            }
            AttributeAssignmentErrorKind::TypeMismatch(target_ty) => {
                // TODO: This is not a very helpful error message for union/intersection, as it does not include the underlying reason
                // why the assignment is invalid. This would be a good use case for sub-diagnostics.
                report_invalid_attribute_assignment(
                    db,
                    builder,
                    target_ty,
                    assignment_value,
                    attribute,
                );
            }
            AttributeAssignmentErrorKind::CannotAssign => {
                builder.into_diagnostic(format_args!(
                    "Cannot assign to attribute `{attribute}` on type `{}`",
                    object_assigned_to.display(db),
                ));
            }
            AttributeAssignmentErrorKind::CannotAssignToClassVar => {
                builder.into_diagnostic(format_args!(
                    "Cannot assign to ClassVar `{attribute}` \
                        from an instance of type `{ty}`",
                    ty = object_assigned_to.display(db),
                ));
            }
            AttributeAssignmentErrorKind::CannotAssignToInstanceAttr => {
                builder.into_diagnostic(format_args!(
                    "Cannot assign to instance attribute \
                        `{attribute}` from the class object `{ty}`",
                    ty = object_assigned_to.display(db),
                ));
            }
            AttributeAssignmentErrorKind::CannotAssignToFinal => {
                builder.into_diagnostic(format_args!(
                    "Cannot assign to final attribute `{attribute}` on type `{ty}`",
                    ty = object_assigned_to.display(db),
                ));
            }
            AttributeAssignmentErrorKind::CannotAssignToUnresolved => {
                builder.into_diagnostic(format!(
                    "Can not assign to unresolved attribute `{attribute}` on type `{ty}`",
                    ty = object_assigned_to.display(db),
                ));
            }
            AttributeAssignmentErrorKind::ReadOnlyProperty(property) => {
                report_attempted_write_to_read_only_property(
                    db,
                    builder,
                    property,
                    attribute,
                    object_assigned_to,
                );
            }
            AttributeAssignmentErrorKind::FailToSet => {
                // TODO: Here, it would be nice to emit an additional diagnostic that explains why the call failed
                builder.into_diagnostic(format_args!(
                    "Invalid assignment to data descriptor attribute \
                        `{attribute}` on type `{}` with custom `__set__` method",
                    object_assigned_to.display(db)
                ));
            }
            AttributeAssignmentErrorKind::FailToSetAttr => {
                builder.into_diagnostic(format_args!(
                    "Can not assign object of type `{}` to attribute \
                        `{attribute}` on type `{}` with custom `__setattr__` method.",
                    assignment_value.display(db),
                    object_assigned_to.display(db)
                ));
            }
            AttributeAssignmentErrorKind::SetAttrReturnsNeverOrNoReturn => {
                builder.into_diagnostic(format_args!(
                    "Cannot assign to attribute `{attribute}` on type `{}` \
                        whose `__setattr__` method returns `Never`/`NoReturn`",
                    object_assigned_to.display(db)
                ));
            }
            AttributeAssignmentErrorKind::Unresolved => {
                builder.into_diagnostic(format_args!(
                    "Unresolved attribute `{attribute}` on type `{}`.",
                    object_assigned_to.display(db)
                ));
            }
        }
    }
}
