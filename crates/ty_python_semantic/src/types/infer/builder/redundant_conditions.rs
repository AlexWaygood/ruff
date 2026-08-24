//! Analysis of whether a boolean test should be reported as being unintentionally
//! always-true or always-false.

use std::borrow::Cow;

use ruff_db::{
    diagnostic::{Annotation, Span},
    parsed::parsed_module,
    source::source_text,
};
use ruff_diagnostics::{Applicability, Edit, Fix};
use ruff_python_ast::{
    self as ast, PythonVersion, helpers::any_over_expr, name::Name, token::parenthesized_range,
};
use ruff_python_trivia::indentation_at_offset;
use ruff_source_file::{LineRanges, find_newline};
use ruff_text_size::{Ranged, TextRange};
use ty_module_resolver::{KnownModule, SearchPath, file_to_module};
use ty_python_core::{
    ProgramFile, Truthiness,
    definition::{Definition, DefinitionKind},
    scope::{NodeWithScopeKind, ScopeId, ScopeKind},
    semantic_index,
};

use crate::{
    Db, Program, ProgramEnvironment, SemanticModel,
    types::{
        KnownClass, LintDiagnosticGuard, Type, TypeContext,
        call::bind::CallableDescription,
        definition_resolution::{
            ImportAliasResolution, ResolvedDefinition, definitions_for_attribute,
            definitions_for_name,
        },
        diagnostic::{REDUNDANT_CONDITION, REDUNDANT_CONDITION_STRICT},
        infer::{InferenceFlags, TypeInferenceBuilder},
        infer_definition_types, infer_expression_types, infer_scope_types,
        tuple::{Tuple, TupleLength},
    },
};

impl<'db> TypeInferenceBuilder<'db, '_> {
    /// Returns whether the current file should be checked for either redundant-condition rule.
    ///
    /// Avoids analyzing excluded files or checking conditions when both rules are disabled.
    pub(super) fn should_check_condition_redundancy(&self) -> bool {
        if !self.db().should_check_file(self.file()) {
            return false;
        }

        if self.file().is_stub(self.db()) {
            return false;
        }

        self.context.is_lint_enabled(&REDUNDANT_CONDITION)
            || self.context.is_lint_enabled(&REDUNDANT_CONDITION_STRICT)
    }

    /// Reports an unintentionally always-truthy or always-falsy condition.
    ///
    /// Whether `redundant-condition` or `redundant-condition-strict` is used depends on two
    /// things:
    /// - The inferred type of the condition. If the type is assignable to `int`, including `bool`,
    ///   `redundant-condition-strict` is used. Otherwise, `redundant-condition` is used.
    /// - Whether any eagerly evaluated walrus expressions appear inside the condition. Many
    ///   expressions can have side effects, but walrus expressions *always* have side effects,
    ///   so the chances that the user is *deliberately* using an always-truthy condition for the
    ///   sole benefit of the side effect is much greater. These are therefore always reported under
    ///   `redundant-condition-strict` to avoid the enabled-by-default rule being overly opinionated.
    ///
    /// Many exemptions are applied to the rule to avoid reporting deliberate uses of always-true
    /// or always-false conditions:
    /// - We exempt conditions where any sub-expression is inferred as being `sys.version_info`,
    ///   `sys.platform`, `os.name`, or `typing.TYPE_CHECKING`. This detection is recursive: if
    ///   any subexpression of the condition is a name or attribute expression, we examine the
    ///   definitions of that name or attribute to see if any subexpresions of those definitions
    ///   is one of those special-cased symbols.
    /// - We exempt conditions using AST literals such as `if True:`, `if 1`, `if 0` and `if False`.
    ///   If one of these is being employed, it's almost certain that the condition is deliberately
    ///   always true or always false.
    /// - We exempt conditions that are part of a suite that is deliberately unreachable, such as
    ///   a defensive exit or exhaustiveness check. This is determined by examining the final
    ///   statement of the suite for a `raise`, a potentially failing assertion, a call returning
    ///   `Never`, or `return NotImplemented`. If the final statement is an `if` with an `else`
    ///   clause, we also allow the suite to be recognized as deliberately unreachable if all of
    ///   the `if`, `elif` and `else` clauses end in terminal statements, recursively.
    ///
    /// Returns the diagnostic guard when the complete condition is reported so callers can attach
    /// additional help or fixes before the guard publishes the diagnostic on drop.
    pub(super) fn check_condition_redundancy<'a>(
        &'a self,
        test: &ast::Expr,
        test_type: Type<'db>,
        test_truthiness: Truthiness,
    ) -> Option<LintDiagnosticGuard<'a, 'a>> {
        if test_truthiness == Truthiness::Ambiguous && !test.is_bool_op_expr() {
            return None;
        }

        let db = self.db();
        let env = self.program_environment();
        let int_instance = KnownClass::Int.to_instance(db, env);

        match test {
            // If they literally have `if False:` in the source code, it's almost certainly deliberate;
            // don't report it as a redundant condition. It's probably there fore debugging or something.
            ast::Expr::BooleanLiteral(_) => return None,

            // Same for `if 0:`
            ast::Expr::NumberLiteral(ast::ExprNumberLiteral {
                value: ast::Number::Int(_),
                ..
            }) => return None,

            // Python checks the truthiness of all but the final `and`/`or` operand to decide
            // whether to short-circuit. If evaluation reaches the final operand, its value is
            // simply returned. Accordingly, `infer_boolean_expression` passes the earlier
            // operands to this method, but never passes the complete expression it is inferring.
            //
            // Receiving the complete `ast::Expr::BoolOp` expression here means a surrounding
            // context, such as an `if`, a `while`, or an outer `and`/`or`, is checking its
            // truthiness. This distinction determines whether the final operand also needs
            // checking:
            //
            // - In `result = flag and func`, `func` is merely a possible result. Its truthiness
            //   is not checked, so it should not produce a diagnostic.
            // - In `if flag and func`, the `if` checks `func` when `flag` is truthy, so the
            //   uncalled function should produce a diagnostic even though the complete
            //   condition has ambiguous truthiness.
            //
            // Check the final operand whenever the complete expression reaches this method;
            // `infer_boolean_expression` has already checked the earlier operands. For values
            // handled by `redundant-condition`, these operand checks are sufficient: checking the
            // complete expression again would duplicate a diagnostic. Values assignable to `int`,
            // including booleans, use `redundant-condition-strict` instead. That rule suppresses
            // diagnostics on subexpressions of conditions, so the complete expression still needs
            // to be checked.
            ast::Expr::BoolOp(ast::ExprBoolOp { values, .. }) => {
                if let Some(last) = values.last() {
                    let ty = self.expression_type(last);
                    self.check_condition_redundancy(last, ty, ty.bool(db, env));
                }

                if !test_type.is_assignable_to(db, env, int_instance) {
                    return None;
                }
            }

            // A negated condition reaches this method twice: `infer_unary_expression_type`
            // checks the operand, and the enclosing `if` or `while` checks the whole condition.
            // Whether the second check should produce a diagnostic depends on the operand:
            //
            // - For `if not func`, the operand check already reports the uncalled function under
            //   `redundant-condition`. Checking the boolean `not func` as well would add a
            //   duplicate `redundant-condition-strict` diagnostic.
            // - For `if not False` or `if not 0`, the operand would use the strict rule. That rule
            //   skips subexpressions of conditions to avoid reporting both a condition and its
            //   parts, so the operand check emits nothing. Checking the whole `not` expression is
            //   therefore necessary to report the redundant condition.
            //
            // Check the whole condition only when the original operand is boolean- or
            // integer-like. Unwrap every `not` first: in `if not not func`, the immediate operand
            // has type `bool`, but the original operand is still `func` and was already reported.
            ast::Expr::UnaryOp(ast::ExprUnaryOp {
                op: ast::UnaryOp::Not,
                operand,
                ..
            }) => {
                let mut original_operand = operand;
                while let ast::Expr::UnaryOp(ast::ExprUnaryOp {
                    op: ast::UnaryOp::Not,
                    operand,
                    ..
                }) = &**original_operand
                {
                    original_operand = operand;
                }

                if !self
                    .expression_type(original_operand)
                    .is_assignable_to(db, env, int_instance)
                {
                    return None;
                }
            }
            _ => {}
        }

        if test_truthiness == Truthiness::Ambiguous {
            return None;
        }

        // Scan deferred bodies, too: a surrounding call may execute a lambda or consume a generator.
        // We do not try to determine when this happens. Avoiding false positives is more important
        // than avoiding false negatives, both for environment exemptions and walrus classification.
        let rule = if test_type.is_assignable_to(db, env, int_instance) {
            if self
                .index
                .is_assertion_test_or_compound_condition_subexpression(
                    self.scope().file_scope_id(db),
                    test.range(),
                )
            {
                return None;
            }
            if !self.context.is_lint_enabled(&REDUNDANT_CONDITION_STRICT) {
                return None;
            }
            &REDUNDANT_CONDITION_STRICT
        } else if any_over_expr(test, ast::Expr::is_named_expr) {
            if !self.context.is_lint_enabled(&REDUNDANT_CONDITION_STRICT) {
                return None;
            }
            &REDUNDANT_CONDITION_STRICT
        } else {
            if !self.context.is_lint_enabled(&REDUNDANT_CONDITION) {
                return None;
            }
            &REDUNDANT_CONDITION
        };

        let model = SemanticModel::new(db, self.program_file());

        if any_over_expr(test, |expression| {
            is_special_cased_condition_expression(db, self.program_file(), expression, |expr| {
                self.expression_type(expr)
            })
        }) {
            return None;
        }

        let annotate_inferred_type = |diagnostic: &mut LintDiagnosticGuard| {
            diagnostic.set_primary_annotation_message(format_args!(
                "Inferred type is `{}`",
                test_type.display(db, env)
            ));
        };

        let describe_boolean_condition = |diagnostic: &mut LintDiagnosticGuard| {
            let source = source_text(db, self.file());
            let condition = &source[test.range()];
            let is_true = test_truthiness.is_always_true();
            if find_newline(condition).is_some() {
                diagnostic.set_concise_message(format_args!("Condition is always {is_true}"));
            } else {
                diagnostic.set_concise_message(format_args!(
                    "Condition `{condition}` is always {is_true}"
                ));
            }

            if let ast::Expr::Compare(ast::ExprCompare {
                left,
                ops,
                comparators,
                ..
            }) = test
                && ops.len() == 1
                && let [single_comparator] = &**comparators
            {
                for node in [left, single_comparator] {
                    diagnostic.annotate(self.context.secondary(node).message(format_args!(
                        "Has type `{}`",
                        self.expression_type(node).display(db, env)
                    )));
                }
            } else {
                annotate_inferred_type(diagnostic);
            }
        };

        match test_truthiness {
            Truthiness::AlwaysTrue => {
                let builder = self.context.report_lint(rule, test)?;

                let describe_always_truthy_object = |diagnostic: &mut LintDiagnosticGuard| {
                    diagnostic.set_concise_message(format_args!(
                        "Object of type `{}` is always truthy",
                        test_type.display(db, env)
                    ));
                    annotate_inferred_type(diagnostic);
                };

                let function_info = match test_type {
                    Type::FunctionLiteral(function) => {
                        Some((function.signature(db), Cow::Borrowed(&**function.name(db))))
                    }
                    Type::BoundMethod(method) => {
                        let function = method.function(db);
                        Some((
                            method.bound_signatures(db),
                            CallableDescription::defining_class(db, test_type)
                                .map(|class| {
                                    Cow::Owned(format!("{}.{}", class.name(db), function.name(db)))
                                })
                                .unwrap_or(Cow::Borrowed(&**function.name(db))),
                        ))
                    }
                    _ => None,
                };

                if let Some((signature, name)) = function_info {
                    let mut diagnostic = if test_type.is_function_literal() {
                        builder.into_diagnostic(format_args!("Function `{name}` is always truthy"))
                    } else {
                        builder.into_diagnostic(format_args!("Method `{name}` is always truthy"))
                    };

                    // Add a suggestion and fix that they might have meant to call (and possibly
                    // also await) this function.
                    //
                    // It's true that calling the function might not actually fix this diagnostic
                    // if the function returns something that is always truthy. They still probably
                    // meant to call the function, though, so it's still a useful suggestion/fix!

                    // A coroutine return type establishes that calling and awaiting the function
                    // is appropriate. `Any`, `Unknown`, and `Never` do not establish this, even
                    // though they are assignable to `CoroutineType`.
                    // Use the top materialization so the unspecified generic arguments do not
                    // prevent concrete coroutine types from being subtypes.
                    let coroutine = KnownClass::CoroutineType
                        .to_instance(db, env)
                        .top_materialization(db, env);
                    let is_awaitable_coro_function = self.can_await_here()
                        && signature.iter().any(|signature| {
                            !signature.return_ty.is_never()
                                && signature.return_ty.is_subtype_of(db, env, coroutine)
                        });

                    let kind = if test_type.is_function_literal() {
                        "function"
                    } else {
                        "method"
                    };

                    if is_awaitable_coro_function {
                        diagnostic.set_primary_annotation_message(format_args!(
                            "Did you mean to `await` and call this {kind}?",
                        ));
                    } else {
                        diagnostic.set_primary_annotation_message(format_args!(
                            "Did you mean to call this {kind}?"
                        ));
                    }

                    if matches!(test, ast::Expr::Name(_) | ast::Expr::Attribute(_)) {
                        let has_parameters = signature.has_parameters();

                        let fix = if is_awaitable_coro_function {
                            let (first, second, applicability) =
                                if test.precedence() <= ast::OperatorPrecedence::Await {
                                    if has_parameters {
                                        ("await (", "(...))", Applicability::DisplayOnly)
                                    } else {
                                        ("await (", "())", Applicability::Unsafe)
                                    }
                                } else {
                                    if has_parameters {
                                        ("await ", "(...)", Applicability::DisplayOnly)
                                    } else {
                                        ("await ", "()", Applicability::Unsafe)
                                    }
                                };
                            Fix::applicable_edits(
                                Edit::insertion(first.to_string(), test.start()),
                                [Edit::insertion(second.to_string(), test.end())],
                                applicability,
                            )
                        } else {
                            let (edit, applicability) = if has_parameters {
                                ("(...)", Applicability::DisplayOnly)
                            } else {
                                ("()", Applicability::Unsafe)
                            };
                            Fix::applicable_edit(
                                Edit::insertion(edit.to_string(), test.end()),
                                applicability,
                            )
                        };
                        diagnostic.set_fix(fix);
                    }

                    Some(diagnostic)
                } else if let Some(tuple_spec) = test_type.tuple_instance_spec(db, env)
                    && tuple_spec.len().minimum() > 0
                {
                    // This error message might not be 100% accurate for a tuple subclass
                    // that overrides `__len__` or `__bool__` in a way that's inconsistent
                    // with the tuple's inherited tuple spec, but you just shouldn't do that anyway.

                    let length = tuple_spec.len();
                    let mut diagnostic = match length {
                        TupleLength::Fixed(size) => builder.into_diagnostic(format_args!(
                            "A {size}-element tuple is always truthy"
                        )),
                        TupleLength::Variable(min, _) => builder.into_diagnostic(format_args!(
                            "A tuple with >={min} element{maybe_s} is always truthy",
                            maybe_s = if min == 1 { "" } else { "s" }
                        )),
                    };
                    describe_always_truthy_object(&mut diagnostic);

                    // The ellipsis suggestion is for `tuple[T]`, not named tuples or other
                    // subclasses whose fixed length is part of their definition.
                    if length == TupleLength::Fixed(1)
                        && test_type.exact_tuple_instance_spec(db).is_some()
                        && let Tuple::Fixed(fixed_length_tuple) = &*tuple_spec
                        && matches!(test, ast::Expr::Name(_) | ast::Expr::Attribute(_))
                    {
                        let definition_info =
                            condition_definition_info(db, self.program_file(), test, |expr| {
                                self.expression_type(expr)
                            });

                        if let Some(single_definition) = definition_info.single_definition {
                            let file = single_definition.python_file(db);
                            let program_file = single_definition.program_file(db);
                            let module = parsed_module(db, file).load(db);
                            let annotation_info = match single_definition.kind(db) {
                                DefinitionKind::AnnotatedAssignment(assignment) => {
                                    let annotation = assignment.annotation(&module);
                                    let annotation_type =
                                        infer_definition_types(db, single_definition)
                                            .try_expression_type(annotation)?;
                                    Some((annotation, annotation_type))
                                }
                                DefinitionKind::Parameter(parameter) => {
                                    parameter.annotation(&module).and_then(|annotation| {
                                        let scope =
                                            single_definition.scope(db).scope(db).parent()?;
                                        let annotation_type = infer_scope_types(
                                            db,
                                            scope.to_scope_id(db, program_file),
                                            TypeContext::default(),
                                        )
                                        .try_expression_type(annotation)?;
                                        Some((annotation, annotation_type))
                                    })
                                }
                                _ => None,
                            };
                            if let Some((annotation, annotation_type)) = annotation_info
                                && annotation_type == test_type
                            {
                                let file = single_definition.file(db);
                                let diagnostic_annotation = || {
                                    Annotation::secondary(
                                        Span::from(file).with_range(annotation.range()),
                                    )
                                };
                                diagnostic.annotate(diagnostic_annotation().message(
                                    "Inferred as a 1-element tuple due to this annotation",
                                ));

                                let resolver_file =
                                    single_definition.program_file(db).resolver_file(db);

                                let sole_element = fixed_length_tuple.elements_slice()[0];
                                let suggested_type = Type::homogeneous_tuple(db, env, sole_element);

                                let annotated_in_first_party_code = file == self.file()
                                    || file_to_module(db, resolver_file)
                                        .and_then(|module| module.search_path(db))
                                        .is_some_and(SearchPath::is_first_party);

                                let annotation = if annotated_in_first_party_code {
                                    diagnostic_annotation().message(format_args!(
                                        "Did you mean `{}`?",
                                        suggested_type.display(db, env)
                                    ))
                                } else {
                                    diagnostic_annotation().message(format_args!(
                                        "The author of this code might have meant `{}`?",
                                        suggested_type.display(db, env)
                                    ))
                                };

                                diagnostic.annotate(annotation);
                            }
                        }
                    }
                    Some(diagnostic)
                } else if let Type::TypedDict(typed_dict) = test_type
                    && let Some(field) = typed_dict
                        .items(db)
                        .iter()
                        .find_map(|(_, field)| field.is_required().then_some(field))
                {
                    let num_required_keys = typed_dict
                        .items(db)
                        .iter()
                        .filter(|(_, field)| field.is_required())
                        .count();
                    let maybe_s = if num_required_keys == 1 { "" } else { "s" };
                    let mut diagnostic = builder.into_diagnostic(format_args!(
                        "A TypedDict with {num_required_keys} required field{maybe_s} is always truthy"
                    ));
                    if let Some(class) = typed_dict.defining_class() {
                        diagnostic.set_concise_message(format_args!(
                            "TypedDict `{}` with {num_required_keys} required field{maybe_s} is always truthy",
                            class.name(db)
                        ));
                    } else {
                        diagnostic.set_concise_message(format_args!(
                            "A TypedDict with {num_required_keys} required field{maybe_s} is always truthy"
                        ));
                    }
                    annotate_inferred_type(&mut diagnostic);
                    if let Some(defining_class) = typed_dict.defining_class()
                        && let Some(typed_dict_definition) = defining_class.definition(db)
                        && let Some(field_definition) = field.first_declaration()
                    {
                        let typed_dict_module =
                            parsed_module(db, typed_dict_definition.python_file(db)).load(db);
                        let field_module =
                            parsed_module(db, field_definition.python_file(db)).load(db);
                        diagnostic.annotate(
                            Annotation::secondary(Span::from(
                                typed_dict_definition.focus_range(db, &typed_dict_module),
                            ))
                            .message(format_args!("`{}` defined here", defining_class.name(db))),
                        );
                        diagnostic.annotate(
                            Annotation::secondary(Span::from(
                                field_definition.full_range(db, &field_module),
                            ))
                            .message(if num_required_keys == 1 {
                                "Required field declared here"
                            } else {
                                "First required field defined here"
                            }),
                        );
                    }
                    Some(diagnostic)
                } else if test_type.as_nominal_instance().is_some_and(|instance| {
                    instance
                        .class(db, env)
                        .is_known(db, KnownClass::GeneratorType)
                }) {
                    let mut diagnostic = builder.into_diagnostic("A generator is always truthy");
                    describe_always_truthy_object(&mut diagnostic);
                    diagnostic.help("Did you mean to collect the generator into a tuple?");
                    if model.definitely_has_builtin_binding("tuple", test.into()) {
                        diagnostic.set_fix(Fix::display_only_edits(
                            Edit::insertion("tuple(".to_string(), test.start()),
                            [Edit::insertion(")".to_string(), test.end())],
                        ));
                    }
                    Some(diagnostic)
                } else if test_type.is_string_literal()
                    || test_type
                        .as_union()
                        .is_some_and(|union| union.elements(db).iter().all(Type::is_string_literal))
                {
                    let mut diagnostic =
                        builder.into_diagnostic("A nonempty string is always truthy");
                    describe_always_truthy_object(&mut diagnostic);
                    Some(diagnostic)
                } else if test_type.is_subtype_of(db, env, KnownClass::Bool.to_instance(db, env)) {
                    let message = "Condition is always true";
                    let mut diagnostic = builder.into_diagnostic(message);
                    describe_boolean_condition(&mut diagnostic);
                    Some(diagnostic)
                } else {
                    let mut diagnostic = builder.into_diagnostic("Condition is always truthy");
                    diagnostic.set_concise_message(format_args!(
                        "Object of type `{}` is always truthy",
                        test_type.display(db, env)
                    ));
                    annotate_inferred_type(&mut diagnostic);
                    if !test_type.is_never()
                        && test_type.try_await(db, env).is_ok()
                        && self.can_await_here()
                    {
                        diagnostic.help("Did you mean to `await` this expression?");

                        let fix = if test.precedence() <= ast::OperatorPrecedence::Await {
                            Fix::unsafe_edits(
                                Edit::insertion("await (".to_string(), test.start()),
                                [Edit::insertion(")".to_string(), test.end())],
                            )
                        } else {
                            Fix::unsafe_edit(Edit::insertion("await ".to_string(), test.start()))
                        };

                        diagnostic.set_fix(fix);
                    }
                    Some(diagnostic)
                }
            }
            Truthiness::AlwaysFalse => {
                let builder = self.context.report_lint(rule, test)?;
                if test_type.is_none(db) {
                    Some(builder.into_diagnostic("`None` is always falsy"))
                } else if let Some(tuple) = test_type.tuple_instance_spec(db, env)
                    && tuple.len() == TupleLength::Fixed(0)
                {
                    // This error message might not be 100% accurate for a tuple subclass
                    // that overrides `__len__` or `__bool__` in a way that's inconsistent
                    // with the tuple's inherited tuple spec, but you just shouldn't do that anyway.
                    let message = "An empty tuple is always falsy";
                    let mut diagnostic = builder.into_diagnostic(message);
                    diagnostic.set_concise_message(message);
                    annotate_inferred_type(&mut diagnostic);
                    Some(diagnostic)
                } else if test_type.is_string_literal() {
                    let message = "An empty string is always falsy";
                    let mut diagnostic = builder.into_diagnostic(message);
                    diagnostic.set_concise_message(message);
                    annotate_inferred_type(&mut diagnostic);
                    Some(diagnostic)
                } else {
                    let is_bool =
                        test_type.is_subtype_of(db, env, KnownClass::Bool.to_instance(db, env));
                    let message = if is_bool {
                        "Condition is always false"
                    } else {
                        "Condition is always falsy"
                    };
                    let mut diagnostic = builder.into_diagnostic(message);
                    if is_bool {
                        describe_boolean_condition(&mut diagnostic);
                    } else {
                        diagnostic.set_concise_message(format_args!(
                            "Object of type `{}` is always falsy",
                            test_type.display(db, env)
                        ));
                        annotate_inferred_type(&mut diagnostic);
                    }
                    Some(diagnostic)
                }
            }
            Truthiness::Ambiguous => None,
        }
    }

    /// Returns `true` if adding `await` at the current expression would produce valid Python.
    ///
    /// Accounts for asynchronous functions, notebook cells, annotation restrictions, enclosing
    /// scopes, and the different scoping behavior of comprehensions and generator expressions.
    fn can_await_here(&self) -> bool {
        // Python forbids `await` in annotation nodes.
        if self
            .inference_flags()
            .contains(InferenceFlags::IN_ANNOTATION)
        {
            return false;
        }

        let db = self.db();

        // A list, set, or dictionary comprehension inherits an enclosing annotation's restriction.
        // A generator expression in between creates its own scope where `await` is valid.
        let mut comprehension_in_annotation = false;
        let mut in_eager_comprehension = false;

        for (scope_id, scope) in self.index.ancestor_scopes(self.scope().file_scope_id(db)) {
            // Before Python 3.11, awaiting in a nested list, set, or dict comprehension cannot
            // implicitly make its containing comprehension or generator expression asynchronous.
            if in_eager_comprehension
                && scope.kind() == ScopeKind::Comprehension
                && self.program_environment().python_version(db) < PythonVersion::PY311
                && !scope_id.is_async_comprehension(self.index)
            {
                return false;
            }

            match scope.node() {
                NodeWithScopeKind::Function(function) => {
                    return !comprehension_in_annotation && function.node(self.module()).is_async;
                }
                NodeWithScopeKind::Lambda(_)
                | NodeWithScopeKind::Class(_)
                | NodeWithScopeKind::ClassTypeParameters(_)
                | NodeWithScopeKind::FunctionTypeParameters(_)
                | NodeWithScopeKind::TypeAliasTypeParameters(_)
                | NodeWithScopeKind::TypeAlias(_) => {
                    return false;
                }
                NodeWithScopeKind::GeneratorExpression(_) => {
                    return true;
                }
                NodeWithScopeKind::Module => {
                    return !comprehension_in_annotation
                        && source_text(db, self.file()).is_notebook();
                }
                NodeWithScopeKind::DictComprehension(_)
                | NodeWithScopeKind::ListComprehension(_)
                | NodeWithScopeKind::SetComprehension(_) => {
                    comprehension_in_annotation |= scope_id.is_defined_in_annotation(self.index);
                    in_eager_comprehension = true;
                }
            }
        }

        false
    }

    /// Checks the direct `if` and `elif` conditions after a suite's statements have been inferred.
    ///
    /// Suppresses conditions guarding deliberately unreachable branches or trailing defensive
    /// exits, and adds an assertion-based autofix when a final `elif` is unnecessarily always true.
    pub(super) fn check_suite_for_redundant_if_statements(&self, suite: &[ast::Stmt]) {
        let db = self.db();
        let env = self.program_environment();

        for (i, statement) in suite.iter().enumerate() {
            let ast::Stmt::If(ast::StmtIf {
                test,
                body,
                elif_else_clauses,
                ..
            }) = statement
            else {
                continue;
            };

            let test_type = self.expression_type(test);
            let test_truthiness = test_type.bool(db, env);

            // Checking if the suite is deliberately unreachable could potentially be expensive.
            // It's only relevant for the strict check, so we only do the check if:
            // 1. The strict check is enabled, and
            // 2. The test type is assignable to int (including bool), meaning it would actually
            //    trigger the strict check rather than the normal check.
            let should_check_if_suite_deliberately_unreachable = |test_type: Type<'db>| {
                self.context.is_lint_enabled(&REDUNDANT_CONDITION_STRICT)
                    && test_type.is_assignable_to(db, env, KnownClass::Int.to_instance(db, env))
            };

            match test_truthiness {
                Truthiness::Ambiguous => {
                    self.check_condition_redundancy(test, test_type, test_truthiness);
                }
                Truthiness::AlwaysFalse => {
                    if !(should_check_if_suite_deliberately_unreachable(test_type)
                        && self.is_deliberately_unreachable_suite(body))
                    {
                        self.check_condition_redundancy(test, test_type, test_truthiness);
                    }
                }
                Truthiness::AlwaysTrue => match elif_else_clauses.as_slice() {
                    [single] => {
                        if !(single.test.is_none()
                            && should_check_if_suite_deliberately_unreachable(test_type)
                            && self.is_deliberately_unreachable_suite(&single.body))
                        {
                            self.check_condition_redundancy(test, test_type, test_truthiness);
                        }
                    }
                    [] => {
                        if !(should_check_if_suite_deliberately_unreachable(test_type)
                            && self.is_deliberately_unreachable_suite(&suite[i + 1..]))
                        {
                            self.check_condition_redundancy(test, test_type, test_truthiness);
                        }
                    }
                    _ => {
                        self.check_condition_redundancy(test, test_type, test_truthiness);
                    }
                },
            }

            for (elif_i, elif_else) in elif_else_clauses.iter().enumerate() {
                let ast::ElifElseClause {
                    body,
                    test: Some(test),
                    ..
                } = elif_else
                else {
                    break;
                };

                let test_type = self.expression_type(test);
                let test_truthiness = test_type.bool(db, env);

                match test_truthiness {
                    Truthiness::Ambiguous => {
                        self.check_condition_redundancy(test, test_type, test_truthiness);
                    }
                    Truthiness::AlwaysFalse => {
                        if !(should_check_if_suite_deliberately_unreachable(test_type)
                            && self.is_deliberately_unreachable_suite(body))
                        {
                            self.check_condition_redundancy(test, test_type, test_truthiness);
                        }
                    }
                    Truthiness::AlwaysTrue => match elif_else_clauses.get(elif_i + 1) {
                        Some(clause) => {
                            if !(clause.test.is_none()
                                && should_check_if_suite_deliberately_unreachable(test_type)
                                && self.is_deliberately_unreachable_suite(&clause.body))
                            {
                                self.check_condition_redundancy(test, test_type, test_truthiness);
                            }
                        }
                        None => {
                            if !(should_check_if_suite_deliberately_unreachable(test_type)
                                && self.is_deliberately_unreachable_suite(&suite[i + 1..]))
                            {
                                let possible_diagnostic = self.check_condition_redundancy(
                                    test,
                                    test_type,
                                    test_truthiness,
                                );
                                if let Some(mut diagnostic) = possible_diagnostic
                                    && !diagnostic.has_applicable_fix(Applicability::DisplayOnly)
                                    && should_check_if_suite_deliberately_unreachable(test_type)
                                {
                                    diagnostic.help(
                                        "Replace this `elif` with an `else` branch \
                                        that asserts the condition to be `True`",
                                    );
                                    if let Some(fix) =
                                        self.replace_redundant_elif_with_assertion(elif_else, test)
                                    {
                                        diagnostic.set_fix(fix);
                                    }
                                }
                            }
                        }
                    },
                }
            }
        }
    }

    /// Replaces an always-true final `elif` with an `else` branch and a defensive assertion.
    ///
    /// Preserves the original condition, comments, branch indentation, and file-wide line-ending
    /// style. Bare assignment expressions are parenthesized so they remain valid assertion tests.
    /// Returns `None` when the branch has no body, its first statement cannot accommodate a new
    /// indented assertion, or rewriting the header would discard a comment.
    ///
    /// The fix is unsafe because an incorrect static assumption can cause the new assertion to
    /// fail at runtime, and optimized Python execution may remove the assertion entirely.
    fn replace_redundant_elif_with_assertion(
        &self,
        clause: &ast::ElifElseClause,
        test: &ast::Expr,
    ) -> Option<Fix> {
        let first_statement = clause.body.first()?;
        let source = source_text(self.db(), self.file());

        if source.line_start(first_statement.start()) == source.line_start(clause.start()) {
            return None;
        }

        let indentation = indentation_at_offset(first_statement.start(), &source)?;
        let parenthesized_test_range =
            parenthesized_range(test.into(), clause.into(), self.module().tokens());
        let test_range = parenthesized_test_range.unwrap_or(test.range());
        let header_prefix_range = TextRange::new(clause.start(), test_range.start());

        // Ruff caches `CommentRanges` in its indexer, but ty does not. Constructing
        // `CommentRanges` here would scan and index every comment in the file just to check
        // this small range, so inspect the existing tokens directly instead.
        if self
            .module()
            .tokens()
            .in_range(header_prefix_range)
            .iter()
            .any(|token| token.kind().is_comment())
        {
            return None;
        }

        let condition = &source[test_range];
        let assertion_condition = if test.is_named_expr() && parenthesized_test_range.is_none() {
            format!("({condition})")
        } else {
            condition.to_string()
        };
        let line_ending = find_newline(&source)
            .map(|(_, ending)| ending)
            .unwrap_or_default()
            .as_str();

        Some(Fix::unsafe_edits(
            Edit::range_replacement(
                "else".to_string(),
                TextRange::new(clause.start(), test_range.end()),
            ),
            [Edit::insertion(
                format!("assert {assertion_condition}{line_ending}{indentation}"),
                first_statement.start(),
            )],
        ))
    }

    /// Return `true` if `suite` is a sequence of statements that acts as a defensive exit
    /// or exhaustiveness check.
    ///
    /// Concretely, we examine the final statement for a `raise`, a potentially failing
    /// assertion, a call returning `Never`, `return NotImplemented`, or a nested conditional
    /// with an explicit `else`. Earlier setup statements do not prevent the suite from being
    /// recognized.
    fn is_deliberately_unreachable_suite(&self, suite: &[ast::Stmt]) -> bool {
        fn is_deliberately_unreachable_inner<'db>(
            builder: &TypeInferenceBuilder<'db, '_>,
            suite: &[ast::Stmt],
            not_implemented: Type<'db>,
        ) -> bool {
            let db = builder.db();
            let env = builder.program_environment();

            suite.last().is_some_and(|stmt| match stmt {
                ast::Stmt::Raise(_) => true,
                ast::Stmt::Assert(ast::StmtAssert { test, .. }) => {
                    builder.expression_type(test).bool(db, env).may_be_false()
                }
                ast::Stmt::Expr(ast::StmtExpr { value, .. }) if value.is_call_expr() => builder
                    .expression_type(value)
                    .is_equivalent_to(db, env, Type::Never),
                ast::Stmt::Return(ast::StmtReturn {
                    value: Some(expr), ..
                }) => {
                    // Known limitation: `Any` and `Unknown` are also assignable to
                    // `NotImplementedType`, so an ordinary return can suppress a diagnostic here.
                    // We prioritise minimising false positives over minimising false negatives
                    // when recognizing potentially deliberate defensive checks.
                    builder
                        .expression_type(expr)
                        .is_assignable_to(db, env, not_implemented)
                }
                ast::Stmt::If(ast::StmtIf {
                    body,
                    elif_else_clauses,
                    ..
                }) => {
                    elif_else_clauses
                        .last()
                        .is_some_and(|last_clause| last_clause.test.is_none())
                        && is_deliberately_unreachable_inner(builder, body, not_implemented)
                        && elif_else_clauses.iter().all(|clause| {
                            is_deliberately_unreachable_inner(
                                builder,
                                &clause.body,
                                not_implemented,
                            )
                        })
                }
                _ => false,
            })
        }

        let not_implemented =
            KnownClass::NotImplementedType.to_instance(self.db(), self.program_environment());
        is_deliberately_unreachable_inner(self, suite, not_implemented)
    }
}

/// Return `true` if any subexpression in `expression` is recognized as "tainted" by being defined
/// (directly or indirectly) with respect to `sys.version_info`, `sys.platform`, `os.name`, or
/// `typing.TYPE_CHECKING`.
///
/// See the docstring of [`TypeInferenceBuilder::check_condition_redundancy`] for more details.
fn is_special_cased_condition_expression<'db>(
    db: &'db dyn Db,
    file: ProgramFile<'db>,
    expression: &ast::Expr,
    mut expression_type: impl FnMut(&ast::Expr) -> Type<'db>,
) -> bool {
    match expression {
        ast::Expr::Name(ast::ExprName { id, .. }) if id == "TYPE_CHECKING" => return true,
        ast::Expr::Attribute(ast::ExprAttribute { value, attr, .. }) => match &**attr {
            "TYPE_CHECKING" => return true,
            "name" => {
                let value_type = expression_type(value);
                if let Type::ModuleLiteral(module) = value_type
                    && module.module(db).is_known(db, KnownModule::Os)
                {
                    return true;
                }
                if value_type.is_never() {
                    return true;
                }
            }
            "version_info" | "platform" => {
                let value_type = expression_type(value);
                if let Type::ModuleLiteral(module) = value_type
                    && module.module(db).is_known(db, KnownModule::Sys)
                {
                    return true;
                }
                if value_type.is_never() {
                    return true;
                }
            }
            _ => {}
        },
        _ => {}
    }

    // We don't recurse through definitions in a flow-sensitive way, but there isn't really any need to.
    // The main objective here is to avoid false positives. Flow-sensitive definitions of variables/attributes
    // where some paths define the place in terms of `sys.version_info` but other paths don't are pretty rare.
    // It's okay to have a small number of false negatives for these very rare edge cases. Attempting to
    // recurse through definitions in a flow-sensitive way would be significantly more complicated.
    condition_definition_info(db, file, expression, expression_type)
        .contains_special_cased_condition
}

/// Resolves the condition's source definitions using a scope or an already-inferred receiver type.
fn condition_definition_info<'db>(
    db: &'db dyn Db,
    file: ProgramFile<'db>,
    expression: &ast::Expr,
    mut expression_type: impl FnMut(&ast::Expr) -> Type<'db>,
) -> ConditionDefinitionInfo<'db> {
    match expression {
        ast::Expr::Name(name) => {
            let index = semantic_index(db, file);
            let Some(scope) = index.try_expression_scope_id(&ast::ExprRef::Name(name)) else {
                return ConditionDefinitionInfo::default();
            };
            name_condition_definition_info(db, scope.to_scope_id(db, file), name.id.clone())
        }
        ast::Expr::Attribute(attribute) => attribute_condition_definition_info(
            db,
            file.program(db),
            expression_type(&attribute.value),
            attribute.attr.id.clone(),
        ),
        _ => ConditionDefinitionInfo::default(),
    }
}

/// The information needed for condition exemptions and annotation hints.
///
/// Retaining only the unique definition and the provenance result lets both uses share a lookup
/// without caching a potentially large list of bindings.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, get_size2::GetSize, salsa::SalsaValue)]
struct ConditionDefinitionInfo<'db> {
    single_definition: Option<Definition<'db>>,
    contains_special_cased_condition: bool,
}

impl<'db> ConditionDefinitionInfo<'db> {
    /// Summarizes resolved definitions, following assignments to establish environment provenance.
    fn from_definitions(db: &'db dyn Db, definitions: Vec<ResolvedDefinition<'db>>) -> Self {
        let single_definition = match definitions.as_slice() {
            [ResolvedDefinition::Definition(definition)] => Some(*definition),
            _ => None,
        };
        let contains_special_cased_condition = definitions
            .into_iter()
            .filter_map(|resolved| resolved.definition())
            .any(|definition| definition_contains_special_cased_condition(db, definition));
        Self {
            single_definition,
            contains_special_cased_condition,
        }
    }
}

/// Caches definition information across uses of the same name in a scope.
///
/// Name lookup considers every reachable binding, so repeating it for every condition can be
/// quadratic in the number of assignments. Caching only the per-definition traversal does not
/// avoid collecting and resolving those bindings again.
#[salsa::tracked(
    returns(copy),
    cycle_initial = |_, _, _, _| ConditionDefinitionInfo::default(),
    heap_size = ruff_memory_usage::heap_size
)]
// Salsa copies this attribute to both the query wrapper and its inner function. The wrapper
// consumes `name`, so `#[expect]` would produce an unfulfilled lint expectation there.
#[allow(clippy::needless_pass_by_value, reason = "Salsa owns the query key")]
fn name_condition_definition_info<'db>(
    db: &'db dyn Db,
    scope: ScopeId<'db>,
    name: Name,
) -> ConditionDefinitionInfo<'db> {
    ConditionDefinitionInfo::from_definitions(
        db,
        definitions_for_name(db, scope, &name, ImportAliasResolution::ResolveAliases),
    )
}

/// Caches definition information for a member of an already-inferred receiver type.
///
/// Attribute lookup can also repeatedly collect many bindings. Include the receiver type in the
/// key because narrowing or rebinding a receiver can change which definitions its members resolve
/// to. Taking that type from the caller avoids re-entering inference of the use-site scope.
#[salsa::tracked(
    returns(copy),
    cycle_initial = |_, _, _, _, _| ConditionDefinitionInfo::default(),
    heap_size = ruff_memory_usage::heap_size
)]
// Salsa copies this attribute to both the query wrapper and its inner function. The wrapper
// consumes `name`, so `#[expect]` would produce an unfulfilled lint expectation there.
#[allow(clippy::needless_pass_by_value, reason = "Salsa owns the query key")]
fn attribute_condition_definition_info<'db>(
    db: &'db dyn Db,
    program: Program<'db>,
    receiver: Type<'db>,
    name: Name,
) -> ConditionDefinitionInfo<'db> {
    ConditionDefinitionInfo::from_definitions(
        db,
        definitions_for_attribute(
            db,
            &ProgramEnvironment::from_program(program),
            receiver,
            &name,
        ),
    )
}

/// Determines whether a definition originates from an environment-dependent guard.
///
/// Follows aliases recursively and recognizes stub declarations for `sys.version_info`,
/// `sys.platform`, `os.name`, and `typing.TYPE_CHECKING`.
///
/// This Salsa-tracked query reads the definition's AST behind its own incremental boundary, so
/// callers do not depend directly on another file's syntax tree. Cyclic aliases recover as `false`.
#[salsa::tracked(
    returns(copy),
    cycle_initial = |_, _, _| false,
    heap_size = ruff_memory_usage::heap_size
)]
fn definition_contains_special_cased_condition<'db>(
    db: &'db dyn Db,
    definition: Definition<'db>,
) -> bool {
    let module = parsed_module(db, definition.python_file(db)).load(db);
    let definition_kind = definition.kind(db);
    let file = definition.file(db);
    let program_file = definition.program_file(db);

    let in_known_module = |known| {
        file_to_module(db, program_file.resolver_file(db))
            .is_some_and(|module| module.is_known(db, known))
    };

    if let DefinitionKind::AnnotatedAssignment(annotated_assignment) = definition_kind
        && file.is_stub(db)
        && let ast::Expr::Name(ast::ExprName { id, .. }) = annotated_assignment.target(&module)
    {
        match &**id {
            "version_info" | "platform" if in_known_module(KnownModule::Sys) => {
                return true;
            }
            "name" if in_known_module(KnownModule::Os) => {
                return true;
            }
            "TYPE_CHECKING" if in_known_module(KnownModule::Typing) => {
                return true;
            }
            _ => {}
        }
    }

    let Some(value) = definition_kind.value(&module) else {
        return false;
    };

    // Unpacked targets do not retain the types of their shared right-hand side. Read those
    // types from the standalone expression query, without re-entering scope inference.
    let standalone = semantic_index(db, program_file).try_expression(value);
    let mut expression_inference = None;
    let mut definition_inference = None;

    any_over_expr(value, |expression| {
        is_special_cased_condition_expression(db, program_file, expression, |expr| {
            if let Some(standalone) = standalone {
                expression_inference
                    .get_or_insert_with(|| {
                        infer_expression_types(db, standalone, TypeContext::default())
                    })
                    .expression_type(expr)
            } else {
                definition_inference
                    .get_or_insert_with(|| infer_definition_types(db, definition))
                    .expression_type(expr)
            }
        })
    })
}
