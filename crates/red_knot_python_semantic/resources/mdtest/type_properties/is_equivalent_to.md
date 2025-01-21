# Equivalence relation

`is_equivalent_to` implements [the equivalence relation] for fully static types.

Two types `A` and `B` are equivalent iff `A` is a subtype of `B` and `B` is a subtype of `A`.

## Basic

```py
from typing import Any
from typing_extensions import Literal
from knot_extensions import Unknown, is_equivalent_to, static_assert

static_assert(is_equivalent_to(Literal[1, 2], Literal[1, 2]))
static_assert(is_equivalent_to(type[object], type))

static_assert(not is_equivalent_to(Any, Any))
static_assert(not is_equivalent_to(Unknown, Unknown))
static_assert(not is_equivalent_to(Any, None))
static_assert(not is_equivalent_to(Literal[1, 2], Literal[1, 0]))
static_assert(not is_equivalent_to(Literal[1, 2], Literal[1, 2, 3]))
```

## Equivalence is commutative

```py
from typing_extensions import Literal
from knot_extensions import is_equivalent_to, static_assert

static_assert(is_equivalent_to(type, type[object]))
static_assert(not is_equivalent_to(Literal[1, 0], Literal[1, 2]))
static_assert(not is_equivalent_to(Literal[1, 2, 3], Literal[1, 2]))
```

## Differently ordered intersections and unions are equivalent

```py
from knot_extensions import is_equivalent_to, static_assert, Intersection, Not

class P: ...
class Q: ...
class R: ...
class S: ...

static_assert(is_equivalent_to(P | Q | R, P | R | Q))  # 1
static_assert(is_equivalent_to(P | Q | R, Q | P | R))  # 2
static_assert(is_equivalent_to(P | Q | R, Q | R | P))  # 3
static_assert(is_equivalent_to(P | Q | R, R | P | Q))  # 4
static_assert(is_equivalent_to(P | Q | R, R | Q | P))  # 5
static_assert(is_equivalent_to(P | R | Q, Q | P | R))  # 6
static_assert(is_equivalent_to(P | R | Q, Q | R | P))  # 7
static_assert(is_equivalent_to(P | R | Q, R | P | Q))  # 8
static_assert(is_equivalent_to(P | R | Q, R | Q | P))  # 9
static_assert(is_equivalent_to(Q | P | R, Q | R | P))  # 10
static_assert(is_equivalent_to(Q | P | R, R | P | Q))  # 11
static_assert(is_equivalent_to(Q | P | R, R | Q | P))  # 12
static_assert(is_equivalent_to(Q | R | P, R | P | Q))  # 13
static_assert(is_equivalent_to(Q | R | P, R | Q | P))  # 14
static_assert(is_equivalent_to(R | P | Q, R | Q | P))  # 15

static_assert(is_equivalent_to(str | None, None | str))

static_assert(is_equivalent_to(Intersection[P, Q], Intersection[Q, P]))
static_assert(is_equivalent_to(Intersection[Q, Not[P]], Intersection[Not[P], Q]))
static_assert(is_equivalent_to(Intersection[Q, R, Not[P]], Intersection[Not[P], R, Q]))
static_assert(is_equivalent_to(Intersection[Q | R, Not[P | S]], Intersection[Not[S | P], R | Q]))
```

## Tuples containing equivalent but differently ordered unions/intersections are equivalent

```py
from knot_extensions import is_equivalent_to, TypeOf, static_assert, Intersection, Not
from typing import Literal

class P: ...
class Q: ...
class R: ...
class S: ...

static_assert(is_equivalent_to(tuple[P | Q], tuple[Q | P]))
static_assert(is_equivalent_to(tuple[P | None], tuple[None | P]))
static_assert(
    is_equivalent_to(tuple[Intersection[P, Q] | Intersection[R, Not[S]]], tuple[Intersection[Not[S], R] | Intersection[Q, P]])
)
```

## `AlwaysTruthy` and `AlwaysFalsy` equivalences

```py
from typing_extensions import LiteralString, Literal
from knot_extensions import AlwaysTruthy, AlwaysFalsy, Not, is_equivalent_to, static_assert, Intersection

static_assert(is_equivalent_to(AlwaysTruthy | bool, AlwaysTruthy | Literal[False]))
static_assert(is_equivalent_to(Not[AlwaysTruthy] | bool, Not[AlwaysTruthy] | Literal[True]))
static_assert(is_equivalent_to(AlwaysFalsy | bool, AlwaysFalsy | Literal[True]))
static_assert(is_equivalent_to(Not[AlwaysFalsy] | bool, Not[AlwaysFalsy] | Literal[False]))
static_assert(is_equivalent_to(AlwaysTruthy | AlwaysFalsy | bool, AlwaysTruthy | AlwaysFalsy))

static_assert(is_equivalent_to(AlwaysTruthy | LiteralString, AlwaysTruthy | Literal[""]))
static_assert(
    is_equivalent_to(Not[AlwaysTruthy] | LiteralString, Not[AlwaysTruthy] | Intersection[LiteralString, Not[Literal[""]]])
)
static_assert(is_equivalent_to(AlwaysFalsy | LiteralString, AlwaysFalsy | Intersection[LiteralString, Not[Literal[""]]]))
static_assert(is_equivalent_to(Not[AlwaysFalsy] | LiteralString, Not[AlwaysFalsy] | Literal[""]))
static_assert(is_equivalent_to(AlwaysTruthy | AlwaysFalsy | LiteralString, AlwaysTruthy | AlwaysFalsy))

static_assert(is_equivalent_to(AlwaysTruthy | AlwaysFalsy | LiteralString | bool, AlwaysTruthy | AlwaysFalsy))
static_assert(is_equivalent_to(AlwaysTruthy | bool | LiteralString, AlwaysTruthy | Literal[False] | Literal[""]))
static_assert(
    is_equivalent_to(
        AlwaysFalsy | bool | LiteralString, AlwaysFalsy | Literal[True] | Intersection[LiteralString, Not[Literal[""]]]
    )
)
static_assert(is_equivalent_to(Not[AlwaysFalsy] | bool | LiteralString, Not[AlwaysFalsy] | Literal[False] | Literal[""]))

def _(
    a: Literal[True, False] | Intersection[Not[Literal[True]], Not[str]],
    b: Literal[False, True] | Intersection[Not[Literal[True]], Not[str]],
    c: Intersection[Not[Literal[True]], Not[str]] | Literal[True, False],
    d: Intersection[Not[Literal[True]], Not[str]] | Literal[False, True],
    e: Literal[True] | Intersection[Not[Literal[True]], Not[str]] | Literal[False],
    f: Literal[False] | Intersection[Not[Literal[True]], Not[str]] | Literal[True],
):
    reveal_type(a)
    reveal_type(b)
    reveal_type(c)
    reveal_type(d)
    reveal_type(e)
    reveal_type(f)
```

[the equivalence relation]: https://typing.readthedocs.io/en/latest/spec/glossary.html#term-equivalent
