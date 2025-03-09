use std::collections::VecDeque;

use rustc_hash::FxHashSet;

use crate::types::class_base::ClassBase;
use crate::types::{Class, Type};
use crate::Db;

/// The inferred method resolution order of a given class.
///
/// See [`Class::iter_mro`] for more details.
#[derive(PartialEq, Eq, Clone, Debug, salsa::Update)]
pub(super) enum Mro<'db> {
    NoBases,
    FromError,
    SingleBase(ClassBase<'db>),
    Resolved(Box<[ClassBase<'db>]>),
}

impl<'db> Mro<'db> {
    /// Attempt to resolve the MRO of a given class
    ///
    /// In the event that a possible list of bases would (or could) lead to a
    /// `TypeError` being raised at runtime due to an unresolvable MRO, we infer
    /// the MRO of the class as being `[<the class in question>, Unknown, object]`.
    /// This seems most likely to reduce the possibility of cascading errors
    /// elsewhere.
    ///
    /// (We emit a diagnostic warning about the runtime `TypeError` in
    /// [`super::infer::TypeInferenceBuilder::infer_region_scope`].)
    pub(super) fn of_class(db: &'db dyn Db, class: Class<'db>) -> Result<Self, MroError<'db>> {
        let class_bases = class.explicit_bases(db);

        if !class_bases.is_empty() && class.inheritance_cycle(db).is_some() {
            // We emit errors for cyclically defined classes elsewhere.
            // It's important that we don't even try to infer the MRO for a cyclically defined class,
            // or we'll end up in an infinite loop.
            return Ok(Self::FromError);
        }

        match class_bases {
            // `builtins.object` is the special case:
            // the only class in Python that has an MRO with length <2
            [] if class.is_object(db) => Ok(Self::Resolved(Box::from([ClassBase::Class(class)]))),

            // All other classes in Python have an MRO with length >=2.
            // Even if a class has no explicit base classes,
            // it will implicitly inherit from `object` at runtime;
            // `object` will appear in the class's `__bases__` list and `__mro__`:
            //
            // ```pycon
            // >>> class Foo: ...
            // ...
            // >>> Foo.__bases__
            // (<class 'object'>,)
            // >>> Foo.__mro__
            // (<class '__main__.Foo'>, <class 'object'>)
            // ```
            [] => Ok(Self::NoBases),

            // Fast path for a class that has only a single explicit base.
            //
            // This *could* theoretically be handled by the final branch below,
            // but it's a common case (i.e., worth optimizing for),
            // and the `c3_merge` function requires lots of allocations.
            [single_base] => ClassBase::try_from_type(db, *single_base)
                .map(Self::SingleBase)
                .ok_or_else(|| MroError::InvalidBases(Box::from([(0, *single_base)]))),

            // The class has multiple explicit bases.
            //
            // We'll fallback to a full implementation of the C3-merge algorithm to determine
            // what MRO Python will give this class at runtime
            // (if an MRO is indeed resolvable at all!)
            multiple_bases => {
                let mut valid_bases = vec![];
                let mut invalid_bases = vec![];

                for (i, base) in multiple_bases.iter().enumerate() {
                    match ClassBase::try_from_type(db, *base) {
                        Some(valid_base) => valid_bases.push(valid_base),
                        None => invalid_bases.push((i, *base)),
                    }
                }

                if !invalid_bases.is_empty() {
                    return Err(MroError::InvalidBases(invalid_bases.into_boxed_slice()));
                }

                let mut seqs = vec![VecDeque::from([ClassBase::Class(class)])];
                for base in &valid_bases {
                    seqs.push(base.mro(db).collect());
                }
                seqs.push(valid_bases.iter().copied().collect());

                c3_merge(seqs).ok_or_else(|| {
                    let mut seen_bases = FxHashSet::default();
                    let mut duplicate_bases = vec![];
                    for (index, base) in valid_bases
                        .iter()
                        .enumerate()
                        .filter_map(|(index, base)| Some((index, base.into_class()?)))
                    {
                        if !seen_bases.insert(base) {
                            duplicate_bases.push((index, base));
                        }
                    }

                    if duplicate_bases.is_empty() {
                        MroError::UnresolvableMro {
                            bases_list: valid_bases.into_boxed_slice(),
                        }
                    } else {
                        MroError::DuplicateBases(duplicate_bases.into_boxed_slice())
                    }
                })
            }
        }
    }
}

/// Iterator that yields elements of a class's MRO.
///
/// We avoid materialising the *full* MRO unless it is actually necessary:
/// - Materialising the full MRO is expensive
/// - We need to do it for every class in the code that we're checking, as we need to make sure
///   that there are no class definitions in the code we're checking that would cause an
///   exception to be raised at runtime. But the same does *not* necessarily apply for every class
///   in third-party and stdlib dependencies: we never emit diagnostics about non-first-party code.
/// - However, we *do* need to resolve attribute accesses on classes/instances from
///   third-party and stdlib dependencies. That requires iterating over the MRO of third-party/stdlib
///   classes, but not necessarily the *whole* MRO: often just the first element is enough.
///   Luckily we know that for any class `X`, the first element of `X`'s MRO will always be `X` itself.
///   We can therefore avoid resolving the full MRO for many third-party/stdlib classes while still
///   being faithful to the runtime semantics.
///
/// Even for first-party code, where we will have to resolve the MRO for every class we encounter,
/// loading the cached MRO comes with a certain amount of overhead, so it's best to avoid calling the
/// Salsa-tracked [`Class::try_mro`] method unless it's absolutely necessary.
pub(super) struct MroIterator<'db> {
    db: &'db dyn Db,

    /// The class whose MRO we're iterating over
    class: Class<'db>,

    /// Whether or not we've already yielded the first element of the MRO
    first_element_yielded: bool,

    /// Whether or not we've already yielded the last element of the MRO
    /// (which is always `object`).
    last_element_yielded: bool,

    /// Iterator over all elements of the MRO except the first and the last.
    ///
    /// The full MRO is expensive to materialize, so this field is `None`
    /// unless we actually *need* to iterate past the first element of the MRO,
    /// at which point it is lazily materialized.
    middle_elemenets: Option<Box<dyn Iterator<Item = ClassBase<'db>> + 'db>>,
}

impl<'db> MroIterator<'db> {
    pub(super) fn new(db: &'db dyn Db, class: Class<'db>) -> Self {
        Self {
            db,
            class,
            first_element_yielded: false,
            last_element_yielded: false,
            middle_elemenets: None,
        }
    }

    /// Materialize the full MRO of the class.
    /// Return an iterator over that MRO which skips the first element of the MRO.
    fn mro_middle_elements(&mut self) -> impl Iterator<Item = ClassBase<'db>> + '_ {
        debug_assert!(self.first_element_yielded);
        self.middle_elemenets.get_or_insert_with(|| {
            debug_assert!(!self.last_element_yielded);
            match self.class.try_mro(self.db) {
                Ok(Mro::NoBases) => Box::new(std::iter::empty()),
                Ok(Mro::FromError) | Err(_) => Box::new(std::iter::once(ClassBase::unknown())),
                Ok(Mro::Resolved(mro)) => {
                    self.last_element_yielded = true;
                    Box::new(mro.into_iter().skip(1).copied())
                }
                Ok(Mro::SingleBase(base)) => {
                    self.last_element_yielded = true;
                    Box::new(base.mro(self.db))
                }
            }
        })
    }
}

impl<'db> Iterator for MroIterator<'db> {
    type Item = ClassBase<'db>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.first_element_yielded {
            self.first_element_yielded = true;
            return Some(ClassBase::Class(self.class));
        }
        if let Some(element) = self.mro_middle_elements().next() {
            return Some(element);
        }
        if self.last_element_yielded {
            return None;
        }
        self.last_element_yielded = true;
        Some(ClassBase::object(self.db))
    }
}

impl std::iter::FusedIterator for MroIterator<'_> {}

/// Possible ways in which attempting to resolve the MRO of a class might fail.
#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub(super) enum MroError<'db> {
    /// The class inherits from one or more invalid bases.
    ///
    /// To avoid excessive complexity in our implementation,
    /// we only permit classes to inherit from class-literal types,
    /// `Todo`, `Unknown` or `Any`. Anything else results in us
    /// emitting a diagnostic.
    ///
    /// This variant records the indices and types of class bases
    /// that we deem to be invalid. The indices are the indices of nodes
    /// in the bases list of the class's [`StmtClassDef`](ruff_python_ast::StmtClassDef) node.
    /// Each index is the index of a node representing an invalid base.
    InvalidBases(Box<[(usize, Type<'db>)]>),

    /// The class has one or more duplicate bases.
    ///
    /// This variant records the indices and [`Class`]es
    /// of the duplicate bases. The indices are the indices of nodes
    /// in the bases list of the class's [`StmtClassDef`](ruff_python_ast::StmtClassDef) node.
    /// Each index is the index of a node representing a duplicate base.
    DuplicateBases(Box<[(usize, Class<'db>)]>),

    /// The MRO is otherwise unresolvable through the C3-merge algorithm.
    ///
    /// See [`c3_merge`] for more details.
    UnresolvableMro { bases_list: Box<[ClassBase<'db>]> },
}

/// Implementation of the [C3-merge algorithm] for calculating a Python class's
/// [method resolution order].
///
/// [C3-merge algorithm]: https://docs.python.org/3/howto/mro.html#python-2-3-mro
/// [method resolution order]: https://docs.python.org/3/glossary.html#term-method-resolution-order
fn c3_merge(mut sequences: Vec<VecDeque<ClassBase>>) -> Option<Mro> {
    // Most MROs aren't that long...
    let mut mro = Vec::with_capacity(8);

    loop {
        sequences.retain(|sequence| !sequence.is_empty());

        if sequences.is_empty() {
            return Some(Mro::Resolved(mro.into_boxed_slice()));
        }

        // If the candidate exists "deeper down" in the inheritance hierarchy,
        // we should refrain from adding it to the MRO for now. Add the first candidate
        // for which this does not hold true. If this holds true for all candidates,
        // return `None`; it will be impossible to find a consistent MRO for the class
        // with the given bases.
        let mro_entry = sequences.iter().find_map(|outer_sequence| {
            let candidate = outer_sequence[0];

            let not_head = sequences
                .iter()
                .all(|sequence| sequence.iter().skip(1).all(|base| base != &candidate));

            not_head.then_some(candidate)
        })?;

        mro.push(mro_entry);

        // Make sure we don't try to add the candidate to the MRO twice:
        for sequence in &mut sequences {
            if sequence[0] == mro_entry {
                sequence.pop_front();
            }
        }
    }
}
