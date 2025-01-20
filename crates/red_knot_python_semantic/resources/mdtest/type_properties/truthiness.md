# Truthiness

## Calling `bool()` on a type that has statically knowable truthiness

```py
from typing_extensions import Literal, LiteralString
from knot_extensions import AlwaysFalsy, AlwaysTruthy

def _(
    a: Literal[1],
    b: Literal[-1],
    c: Literal["foo"],
    d: tuple[Literal[0]],
    e: Literal[1, 2],
    f: AlwaysTruthy,
):
    reveal_type(bool(a))  # revealed: Literal[True]
    reveal_type(bool(b))  # revealed: Literal[True]
    reveal_type(bool(c))  # revealed: Literal[True]
    reveal_type(bool(d))  # revealed: Literal[True]
    reveal_type(bool(e))  # revealed: Literal[True]
    reveal_type(bool(f))  # revealed: Literal[True]

def _(
    a: tuple[()],
    b: Literal[0],
    c: Literal[""],
    d: Literal[b""],
    e: Literal[0, 0],
    f: AlwaysFalsy,
):
    reveal_type(bool(a))  # revealed: Literal[False]
    reveal_type(bool(b))  # revealed: Literal[False]
    reveal_type(bool(c))  # revealed: Literal[False]
    reveal_type(bool(d))  # revealed: Literal[False]
    reveal_type(bool(e))  # revealed: Literal[False]
    reveal_type(bool(f))  # revealed: Literal[False]

def _(
    a: str,
    b: Literal[1, 0],
    c: str | Literal[0],
    d: str | Literal[1],
):
    reveal_type(bool(a))  # revealed: bool
    reveal_type(bool(b))  # revealed: bool
    reveal_type(bool(c))  # revealed: bool
    reveal_type(bool(d))  # revealed: bool
```

## Equivalent types

```py
from knot_extensions import static_assert, is_equivalent_to, AlwaysTruthy, AlwaysFalsy, Not, Intersection
from typing_extensions import Literal, LiteralString

static_assert(is_equivalent_to(AlwaysTruthy | bool, AlwaysTruthy | Literal[False]))
static_assert(is_equivalent_to(AlwaysFalsy | bool, AlwaysFalsy | Literal[True]))

# TODO: these should pass
# error: [static-assert-error]
static_assert(is_equivalent_to(AlwaysTruthy | LiteralString, AlwaysTruthy | Literal[""]))
# error: [static-assert-error]
static_assert(is_equivalent_to(AlwaysTruthy | LiteralString, Literal[""] | AlwaysTruthy))
# error: [static-assert-error]
static_assert(is_equivalent_to(LiteralString | AlwaysTruthy, AlwaysTruthy | Literal[""]))
# error: [static-assert-error]
static_assert(is_equivalent_to(LiteralString | AlwaysTruthy, Literal[""] | AlwaysTruthy))

# TODO: these should pass
# error: [static-assert-error]
static_assert(is_equivalent_to(Not[AlwaysFalsy] | LiteralString, Not[AlwaysFalsy] | Literal[""]))
# error: [static-assert-error]
static_assert(is_equivalent_to(Not[AlwaysFalsy] | LiteralString, Literal[""] | Not[AlwaysFalsy]))
# error: [static-assert-error]
static_assert(is_equivalent_to(LiteralString | Not[AlwaysFalsy], Not[AlwaysFalsy] | Literal[""]))
# error: [static-assert-error]
static_assert(is_equivalent_to(LiteralString | Not[AlwaysFalsy], Literal[""] | Not[AlwaysFalsy]))

# TODO: these should pass
# error: [static-assert-error]
static_assert(is_equivalent_to(AlwaysTruthy | AlwaysFalsy | LiteralString, AlwaysTruthy | AlwaysFalsy))
# error: [static-assert-error]
static_assert(is_equivalent_to(AlwaysTruthy | AlwaysFalsy | LiteralString, AlwaysFalsy | AlwaysTruthy))
# error: [static-assert-error]
static_assert(is_equivalent_to(AlwaysFalsy | AlwaysTruthy | LiteralString, AlwaysTruthy | AlwaysFalsy))
# error: [static-assert-error]
static_assert(is_equivalent_to(AlwaysFalsy | AlwaysTruthy | LiteralString, AlwaysFalsy | AlwaysTruthy))
# error: [static-assert-error]
static_assert(is_equivalent_to(LiteralString | AlwaysFalsy | AlwaysTruthy, AlwaysTruthy | AlwaysFalsy))
# error: [static-assert-error]
static_assert(is_equivalent_to(LiteralString | AlwaysFalsy | AlwaysTruthy, AlwaysFalsy | AlwaysTruthy))
# error: [static-assert-error]
static_assert(is_equivalent_to(LiteralString | AlwaysTruthy | AlwaysFalsy, AlwaysTruthy | AlwaysFalsy))
# error: [static-assert-error]
static_assert(is_equivalent_to(LiteralString | AlwaysTruthy | AlwaysFalsy, AlwaysFalsy | AlwaysTruthy))
```
