# Detection of boolean tests that are always truthy or always falsy

A common error in Python is to accidentally test truthiness of the wrong object; for example
`if func:` (which is always true) where `if func():` was intended, or `if coroutine():` where
`if await coroutine():` was intended. By default, ty alerts the user to these errors with the error
code `redundant-condition`, but only if the inferred type of the object is not assignable to `int`.
This heuristic catches the `if func` and `if coroutine()` cases, while avoiding false positives on
cases such as `if DEBUG:` where `DEBUG = 0` or `DEBUG = False` is a constant.

The remaining cases -- where the inferred type is assignable to `int` -- are covered by a separate,
stricter rule (`redundant-condition-strict`).

```toml
[environment]
python-version = "3.14"
python-platform = "linux"

[rules]
redundant-condition-strict = "error"
```

## Basic cases

We catch testing a function without calling it:

```py
def func(): ...

if func:  # snapshot: redundant-condition
    pass
```

```snapshot
warning[redundant-condition]: Function `func` is always truthy
 --> src/mdtest_snippet.py:3:4
  |
3 | if func:  # snapshot: redundant-condition
  |    ^^^^ Did you mean to call this function?
  |
2 |
  - if func:  # snapshot: redundant-condition
3 + if func():  # snapshot: redundant-condition
4 |     pass
  |
note: This is an unsafe fix and may change runtime behavior
```

And testing a method without calling it:

```py
class Foo:
    def bar(self) -> bool:
        return True

    def baz(self):
        if self.bar:  # snapshot: redundant-condition
            pass
```

```snapshot
warning[redundant-condition]: Method `Foo.bar` is always truthy
  --> src/mdtest_snippet.py:10:12
   |
10 |         if self.bar:  # snapshot: redundant-condition
   |            ^^^^^^^^ Did you mean to call this method?
   |
9  |     def baz(self):
   -         if self.bar:  # snapshot: redundant-condition
10 +         if self.bar():  # snapshot: redundant-condition
11 |             pass
   |
note: This is an unsafe fix and may change runtime behavior
```

And testing a generator expression without executing it:

```py
def work(items: list[int]):
    filtered = (item for item in items if item < 42)
    if filtered:  # snapshot: redundant-condition
        pass
    assert filtered  # error: [redundant-condition] "Object of type `GeneratorType[int, None, None]` is always truthy"
```

```snapshot
warning[redundant-condition]: A generator is always truthy
  --> src/mdtest_snippet.py:14:8
   |
14 |     if filtered:  # snapshot: redundant-condition
   |        ^^^^^^^^ Inferred type is `GeneratorType[int, None, None]`
help: Did you mean to collect the generator into a tuple?
   |
13 |     filtered = (item for item in items if item < 42)
   -     if filtered:  # snapshot: redundant-condition
14 +     if tuple(filtered):  # snapshot: redundant-condition
15 |         pass
   |
note: This is a display-only fix and is likely to be incorrect
```

And testing an awaitable without awaiting it:

```py
async def coroutine(): ...
async def main():
    if coroutine():  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:19:8
   |
19 |     if coroutine():  # snapshot: redundant-condition
   |        ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
   |
18 | async def main():
   -     if coroutine():  # snapshot: redundant-condition
19 +     if await coroutine():  # snapshot: redundant-condition
20 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

And testing a tuple that is known to always be empty or non-empty:

```py
class Foo:
    def __init__(self):
        self.two_element_tuple: tuple[int, int] = (423, 432)
        self.at_least_one_element: tuple[int, *tuple[int, ...]] = (42,)
        self.at_least_two_elements: tuple[int, int, *tuple[int, ...]] = (42, 42)
        self.no_elements: tuple[()] = ()

    def other_method(self):
        if self.two_element_tuple:  # snapshot: redundant-condition
            pass
        if self.at_least_one_element:  # snapshot: redundant-condition
            pass
        if self.at_least_two_elements:  # snapshot: redundant-condition
            pass
        if self.no_elements:  # snapshot: redundant-condition
            pass

        # error: [redundant-condition] "Object of type `tuple[int, *tuple[int, ...]]` is always truthy"
        assert self.at_least_one_element
        # error: [redundant-condition] "Object of type `tuple[int, int, *tuple[int, ...]]` is always truthy"
        assert self.at_least_two_elements
```

```snapshot
warning[redundant-condition]: A 2-element tuple is always truthy
  --> src/mdtest_snippet.py:29:12
   |
29 |         if self.two_element_tuple:  # snapshot: redundant-condition
   |            ^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `tuple[int, int]`


warning[redundant-condition]: A tuple with >=1 element is always truthy
  --> src/mdtest_snippet.py:31:12
   |
31 |         if self.at_least_one_element:  # snapshot: redundant-condition
   |            ^^^^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `tuple[int, *tuple[int, ...]]`


warning[redundant-condition]: A tuple with >=2 elements is always truthy
  --> src/mdtest_snippet.py:33:12
   |
33 |         if self.at_least_two_elements:  # snapshot: redundant-condition
   |            ^^^^^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `tuple[int, int, *tuple[int, ...]]`


warning[redundant-condition]: An empty tuple is always falsy
  --> src/mdtest_snippet.py:35:12
   |
35 |         if self.no_elements:  # snapshot: redundant-condition
   |            ^^^^^^^^^^^^^^^^ Inferred type is `tuple[()]`
```

Annotating a variable as `tuple[X]` is almost always a mistake (the user almost always meant to
write `tuple[X, ...]`), so we emit a specialized error message and autofix for this specific case:

```py
class Bar:
    def __init__(self):
        self.single_element_tuple: tuple[int] = (42,)

    def first_method(self):
        self.single_element_tuple = (56,)

    def other_method(self, y: tuple[str]):
        if self.single_element_tuple:  # snapshot: redundant-condition
            pass

        if y:  # snapshot: redundant-condition
            pass
```

```snapshot
warning[redundant-condition]: A 1-element tuple is always truthy
  --> src/mdtest_snippet.py:50:12
   |
50 |         if self.single_element_tuple:  # snapshot: redundant-condition
   |            ^^^^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `tuple[int]`
   |
  ::: src/mdtest_snippet.py:44:36
   |
44 |         self.single_element_tuple: tuple[int] = (42,)
   |                                    ----------
   |                                    |
   |                                    Inferred as a 1-element tuple due to this annotation
   |                                    Did you mean `tuple[int, ...]`?


warning[redundant-condition]: A 1-element tuple is always truthy
  --> src/mdtest_snippet.py:53:12
   |
49 |     def other_method(self, y: tuple[str]):
   |                               ----------
   |                               |
   |                               Inferred as a 1-element tuple due to this annotation
   |                               Did you mean `tuple[str, ...]`?
50 |         if self.single_element_tuple:  # snapshot: redundant-condition
51 |             pass
52 |
53 |         if y:  # snapshot: redundant-condition
   |            ^ Inferred type is `tuple[str]`
```

And testing `None`:

```py
X = None

if X:  # snapshot: redundant-condition
    pass
```

```snapshot
warning[redundant-condition]: `None` is always falsy
  --> src/mdtest_snippet.py:57:4
   |
57 | if X:  # snapshot: redundant-condition
   |    ^
```

And testing a string that is known to always be truthy or always be falsy:

```py
x = "foo"
y = ""

if x:  # snapshot: redundant-condition
    pass

if y:  # snapshot: redundant-condition
    pass
```

```snapshot
warning[redundant-condition]: A nonempty string is always truthy
  --> src/mdtest_snippet.py:62:4
   |
62 | if x:  # snapshot: redundant-condition
   |    ^ Inferred type is `Literal["foo"]`


warning[redundant-condition]: An empty string is always falsy
  --> src/mdtest_snippet.py:65:4
   |
65 | if y:  # snapshot: redundant-condition
   |    ^ Inferred type is `Literal[""]`
```

or even a union of strings that is known to always be truthy:

```py
from typing import Literal

def f(x: Literal["a", "b"]):
    if x:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: A nonempty string is always truthy
  --> src/mdtest_snippet.py:70:8
   |
70 |     if x:  # snapshot: redundant-condition
   |        ^ Inferred type is `Literal["a", "b"]`
```

and testing a `TypedDict` that is known to always be truthy:

```py
from typing import TypedDict, NotRequired, Required

class NeverEmpty(TypedDict):
    x: int
    y: str

class AlsoNeverEmpty(TypedDict, total=False):
    x: Required[int]

class SometimesEmpty(TypedDict):
    x: NotRequired[int]

class AlsoSometimesEmpty(TypedDict, total=False):
    x: int

def test(
    never_empty: NeverEmpty,
    also_never_empty: AlsoNeverEmpty,
    sometimes_empty: SometimesEmpty,
    also_sometimes_empty: AlsoSometimesEmpty,
):
    if never_empty:  # snapshot: redundant-condition
        pass

    if also_never_empty:  # snapshot: redundant-condition
        pass

    if sometimes_empty:  # no diagnostic
        pass

    if also_sometimes_empty:  # no diagnostic
        pass

    assert never_empty  # error: [redundant-condition] "TypedDict `NeverEmpty` with 2 required fields is always truthy"
    assert also_never_empty  # error: [redundant-condition] "TypedDict `AlsoNeverEmpty` with 1 required field is always truthy"
```

```snapshot
warning[redundant-condition]: A TypedDict with 2 required fields is always truthy
  --> src/mdtest_snippet.py:93:8
   |
93 |     if never_empty:  # snapshot: redundant-condition
   |        ^^^^^^^^^^^ Inferred type is `NeverEmpty`
   |
  ::: src/mdtest_snippet.py:74:7
   |
74 | class NeverEmpty(TypedDict):
   |       ---------- `NeverEmpty` defined here
75 |     x: int
   |     ------ First required field defined here


warning[redundant-condition]: A TypedDict with 1 required field is always truthy
  --> src/mdtest_snippet.py:96:8
   |
96 |     if also_never_empty:  # snapshot: redundant-condition
   |        ^^^^^^^^^^^^^^^^ Inferred type is `AlsoNeverEmpty`
   |
  ::: src/mdtest_snippet.py:78:7
   |
78 | class AlsoNeverEmpty(TypedDict, total=False):
   |       -------------- `AlsoNeverEmpty` defined here
79 |     x: Required[int]
   |     ---------------- Required field declared here
```

## Inherited required `TypedDict` fields

A required field makes a `TypedDict` nonempty even when the field is inherited from a class in
another module. The diagnostic points to the inherited field's declaration in that module.

`base.py`:

```py
from typing import TypedDict

class Base(TypedDict):
    value: int
```

`child.py`:

```py
from base import Base

class Child(Base):
    pass

def check(value: Child):
    if value:  # snapshot: redundant-condition
        pass
    assert value  # error: [redundant-condition] "TypedDict `Child` with 1 required field is always truthy"
```

```snapshot
warning[redundant-condition]: A TypedDict with 1 required field is always truthy
 --> src/child.py:7:8
  |
7 |     if value:  # snapshot: redundant-condition
  |        ^^^^^ Inferred type is `Child`
  |
 ::: src/child.py:3:7
  |
3 | class Child(Base):
  |       ----- `Child` defined here
  |
 ::: src/base.py:4:5
  |
4 |     value: int
  |     ---------- Required field declared here
```

## Required keys established by narrowing

A key-presence check can narrow an open `TypedDict` to an unnamed schema with required keys. The
diagnostic describes the number of required fields without inventing a class name.

```py
from typing import TypedDict

class Record(TypedDict):
    pass

def check(value: Record):
    if "x" in value:
        if value:  # error: [redundant-condition] "A TypedDict with 1 required field is always truthy"
            pass
```

## Tuple annotations in dependencies

A one-element tuple annotation in a dependency explains why the condition is redundant, but we do
not suggest changing that dependency's annotation to an arbitrary-length tuple.

```toml
[environment]
python = "/.venv"
```

`/.venv/<path-to-site-packages>/records.pyi`:

```pyi
values: tuple[str]
```

`main.py`:

```py
import records

if records.values:  # snapshot: redundant-condition
    pass
```

```snapshot
warning[redundant-condition]: A 1-element tuple is always truthy
 --> src/main.py:3:4
  |
3 | if records.values:  # snapshot: redundant-condition
  |    ^^^^^^^^^^^^^^ Inferred type is `tuple[str]`
  |
 ::: .venv/<path-to-site-packages>/records.pyi:1:9
  |
1 | values: tuple[str]
  |         ----------
  |         |
  |         Inferred as a 1-element tuple due to this annotation
  |         The author of this code might have meant `tuple[str, ...]`?
```

## Generator fixes with shadowed builtins

A local binding named `tuple` prevents a generator from being safely wrapped in the builtin tuple
constructor.

`shadowed_locally.py`:

```py
def check(items: list[int], tuple: object):
    generated = (item for item in items)
    if generated:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: A generator is always truthy
 --> src/shadowed_locally.py:3:8
  |
3 |     if generated:  # snapshot: redundant-condition
  |        ^^^^^^^^^ Inferred type is `GeneratorType[int, None, None]`
help: Did you mean to collect the generator into a tuple?
```

A module-level binding also shadows the builtin inside a nested function.

`shadowed_globally.py`:

```py
tuple = 42

def check(items: list[int]):
    generated = (item for item in items)
    if generated:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: A generator is always truthy
 --> src/shadowed_globally.py:5:8
  |
5 |     if generated:  # snapshot: redundant-condition
  |        ^^^^^^^^^ Inferred type is `GeneratorType[int, None, None]`
help: Did you mean to collect the generator into a tuple?
```

Class attributes do not shadow builtins inside methods, so the suggestion remains valid.

`class_attribute.py`:

```py
class Container:
    tuple = 42

    def check(self, items: list[int]):
        generated = (item for item in items)
        if generated:  # snapshot: redundant-condition
            pass
```

```snapshot
warning[redundant-condition]: A generator is always truthy
 --> src/class_attribute.py:6:12
  |
6 |         if generated:  # snapshot: redundant-condition
  |            ^^^^^^^^^ Inferred type is `GeneratorType[int, None, None]`
help: Did you mean to collect the generator into a tuple?
  |
5 |         generated = (item for item in items)
  -         if generated:  # snapshot: redundant-condition
6 +         if tuple(generated):  # snapshot: redundant-condition
7 |             pass
  |
note: This is a display-only fix and is likely to be incorrect
```

## Generator fixes with project-level builtin overrides

A project-level `__builtins__.pyi` can replace `tuple`, so calling that name would not necessarily
collect a generator into a tuple.

```py
def check(items: list[int]):
    generated = (item for item in items)
    if generated:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: A generator is always truthy
 --> src/mdtest_snippet.py:3:8
  |
3 |     if generated:  # snapshot: redundant-condition
  |        ^^^^^^^^^ Inferred type is `GeneratorType[int, None, None]`
help: Did you mean to collect the generator into a tuple?
```

`__builtins__.pyi`:

```pyi
def tuple(value: object) -> bool: ...
```

## Generator fixes with unrelated project-level builtins

A project-level `__builtins__.pyi` that defines another name does not prevent `tuple` from resolving
to the standard builtin.

```py
def check(items: list[int]):
    generated = (item for item in items)
    if generated:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: A generator is always truthy
 --> src/mdtest_snippet.py:3:8
  |
3 |     if generated:  # snapshot: redundant-condition
  |        ^^^^^^^^^ Inferred type is `GeneratorType[int, None, None]`
help: Did you mean to collect the generator into a tuple?
  |
2 |     generated = (item for item in items)
  -     if generated:  # snapshot: redundant-condition
3 +     if tuple(generated):  # snapshot: redundant-condition
4 |         pass
  |
note: This is a display-only fix and is likely to be incorrect
```

`__builtins__.pyi`:

```pyi
custom_builtin: int
```

## Other boolean contexts

Redundant conditions are not merely detected in `if` tests. They are also detected in unary `not`
operations, `while` loops, `if` expressions, `and` expressions, `or` expressions, `match` guards,
and in comprehension `if` tests.

```py
def coinflip() -> bool:
    return True

def func(): ...

if not func:  # error: [redundant-condition]
    pass

if not not func:  # error: [redundant-condition]
    pass

a = True if func else False  # error: [redundant-condition]

if coinflip() if func else False:  # error: [redundant-condition]
    pass

b = func and coinflip()  # error: [redundant-condition]

if func and coinflip():  # error: [redundant-condition]
    pass

c = func or coinflip()  # error: [redundant-condition]

if func or coinflip():  # error: [redundant-condition]
    pass

[x for x in range(3) if func]  # error: [redundant-condition]

def function(flag: bool):
    if flag:
        pass
    elif func:  # error: [redundant-condition]
        pass

assert func  # error: [redundant-condition]

while func and coinflip():  # error: [redundant-condition]
    pass

while not (func and coinflip()):  # error: [redundant-condition]
    pass

def f(x: str | int):
    match x:
        case str() if func:  # error: [redundant-condition]
            pass

# N.B. this `while` statement must come last in the test snippet,
# as ty considers all code following it to be unreachable,
# and does not emit any diagnostics in unreachable code!
#
while func:  # error: [redundant-condition]
    pass
```

## Always truthy values appearing later in compound conditions

A subexpression in a compound condition can be inferred as always truthy or always falsy even if the
condition overall is inferred as having ambiguous truthiness. We still report these subexpressions:

```py
def func(): ...
def compound_statement_conditions(flag: bool, other: bool):
    if flag and func:  # snapshot: redundant-condition
        pass

    if other:
        pass
    elif flag and func:  # error: [redundant-condition]
        pass

    while flag and func:  # error: [redundant-condition]
        break

    match flag:
        case bool() if flag and func:  # error: [redundant-condition]
            pass

def compound_expression_conditions(flag: bool):
    selected = True if flag and func else False  # snapshot: redundant-condition
    filtered = [value for value in range(1) if flag and func]  # error: [redundant-condition]
    result = flag and func

def compound_assertion_condition(flag: bool):
    assert flag and func  # snapshot: redundant-condition
```

```snapshot
warning[redundant-condition]: Function `func` is always truthy
 --> src/mdtest_snippet.py:3:17
  |
3 |     if flag and func:  # snapshot: redundant-condition
  |                 ^^^^ Did you mean to call this function?
  |
2 | def compound_statement_conditions(flag: bool, other: bool):
  -     if flag and func:  # snapshot: redundant-condition
3 +     if flag and func():  # snapshot: redundant-condition
4 |         pass
  |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Function `func` is always truthy
  --> src/mdtest_snippet.py:19:33
   |
19 |     selected = True if flag and func else False  # snapshot: redundant-condition
   |                                 ^^^^ Did you mean to call this function?
   |
18 | def compound_expression_conditions(flag: bool):
   -     selected = True if flag and func else False  # snapshot: redundant-condition
19 +     selected = True if flag and func() else False  # snapshot: redundant-condition
20 |     filtered = [value for value in range(1) if flag and func]  # error: [redundant-condition]
   |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Function `func` is always truthy
  --> src/mdtest_snippet.py:24:21
   |
24 |     assert flag and func  # snapshot: redundant-condition
   |                     ^^^^ Did you mean to call this function?
   |
23 | def compound_assertion_condition(flag: bool):
   -     assert flag and func  # snapshot: redundant-condition
24 +     assert flag and func()  # snapshot: redundant-condition
   |
note: This is an unsafe fix and may change runtime behavior
```

## Edge cases

A nonempty tuple subclass can still be falsy if it overrides `__bool__`:

```py
from typing import Any, Literal, Never
from types import CoroutineType

async def coroutine(): ...

class FalsyTuple(tuple[int, int]):
    def __bool__(self) -> Literal[False]:
        return False

def check_falsy_tuple(value: FalsyTuple):
    if value:  # error: [redundant-condition] "Object of type `FalsyTuple` is always falsy"
        pass
```

Simply calling an asynchronous function would not resolve the redundant condition: the function must
be called *and* awaited, so this is what the autofix suggests:

```py
async def inspect_async_function():
    if coroutine:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Function `coroutine` is always truthy
  --> src/mdtest_snippet.py:14:8
   |
14 |     if coroutine:  # snapshot: redundant-condition
   |        ^^^^^^^^^ Did you mean to `await` and call this function?
   |
13 | async def inspect_async_function():
   -     if coroutine:  # snapshot: redundant-condition
14 +     if await coroutine():  # snapshot: redundant-condition
15 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

Calling a function with an always-truthy return value does not resolve the redundant condition --
but they still probably meant to call the function, so we still offer autofixes in these cases:

```py
def always_truthy() -> Literal[True]:
    return True

def inspect_truthy_function():
    if always_truthy:  # snapshot: redundant-condition
        pass

async def always_truthy_coro() -> Literal[True]:
    return True

async def foo():
    if always_truthy_coro:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Function `always_truthy` is always truthy
  --> src/mdtest_snippet.py:20:8
   |
20 |     if always_truthy:  # snapshot: redundant-condition
   |        ^^^^^^^^^^^^^ Did you mean to call this function?
   |
19 | def inspect_truthy_function():
   -     if always_truthy:  # snapshot: redundant-condition
20 +     if always_truthy():  # snapshot: redundant-condition
21 |         pass
   |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Function `always_truthy_coro` is always truthy
  --> src/mdtest_snippet.py:27:8
   |
27 |     if always_truthy_coro:  # snapshot: redundant-condition
   |        ^^^^^^^^^^^^^^^^^^ Did you mean to `await` and call this function?
   |
26 | async def foo():
   -     if always_truthy_coro:  # snapshot: redundant-condition
27 +     if await always_truthy_coro():  # snapshot: redundant-condition
28 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

If a function has parameters, we still offer a "fix", but we do not attempt to make the fix valid --
it's just to show the user visually what kind of edit we're suggesting that they make. The fix is
"display-only" to indicate that it's almost certainly incorrect:

```py
def wut(x): ...

if wut:  # snapshot: redundant-condition
    pass

async def wuttt(x): ...
async def bar():
    if wuttt:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Function `wut` is always truthy
  --> src/mdtest_snippet.py:31:4
   |
31 | if wut:  # snapshot: redundant-condition
   |    ^^^ Did you mean to call this function?
   |
30 |
   - if wut:  # snapshot: redundant-condition
31 + if wut(...):  # snapshot: redundant-condition
32 |     pass
   |
note: This is a display-only fix and is likely to be incorrect


warning[redundant-condition]: Function `wuttt` is always truthy
  --> src/mdtest_snippet.py:36:8
   |
36 |     if wuttt:  # snapshot: redundant-condition
   |        ^^^^^ Did you mean to `await` and call this function?
   |
35 | async def bar():
   -     if wuttt:  # snapshot: redundant-condition
36 +     if await wuttt(...):  # snapshot: redundant-condition
37 |         pass
   |
note: This is a display-only fix and is likely to be incorrect
```

Synchronous functions returning `Any`, an inferred `Unknown`, or `Never` are not known to return
coroutines. We suggest calling them without adding `await`, even inside an asynchronous function.

```py
def unannotated():
    return False

def dynamic() -> Any:
    return False

def terminate() -> Never:
    raise RuntimeError

async def check_synchronous_functions():
    if unannotated:  # snapshot: redundant-condition
        pass
    if dynamic:  # snapshot: redundant-condition
        pass
    if terminate:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Function `unannotated` is always truthy
  --> src/mdtest_snippet.py:48:8
   |
48 |     if unannotated:  # snapshot: redundant-condition
   |        ^^^^^^^^^^^ Did you mean to call this function?
   |
47 | async def check_synchronous_functions():
   -     if unannotated:  # snapshot: redundant-condition
48 +     if unannotated():  # snapshot: redundant-condition
49 |         pass
   |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Function `dynamic` is always truthy
  --> src/mdtest_snippet.py:50:8
   |
50 |     if dynamic:  # snapshot: redundant-condition
   |        ^^^^^^^ Did you mean to call this function?
   |
49 |         pass
   -     if dynamic:  # snapshot: redundant-condition
50 +     if dynamic():  # snapshot: redundant-condition
51 |         pass
   |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Function `terminate` is always truthy
  --> src/mdtest_snippet.py:52:8
   |
52 |     if terminate:  # snapshot: redundant-condition
   |        ^^^^^^^^^ Did you mean to call this function?
   |
51 |         pass
   -     if terminate:  # snapshot: redundant-condition
52 +     if terminate():  # snapshot: redundant-condition
53 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

A synchronous function can explicitly return a coroutine. Calling and awaiting that function is a
valid suggestion, while testing a call that never returns has no condition to diagnose.

```py
def make_coroutine() -> CoroutineType[Any, Any, bool]:
    return always_truthy_coro()

async def check_coroutine_factory():
    if make_coroutine:  # snapshot: redundant-condition
        pass

async def check_nonreturning_call():
    if terminate():
        pass
```

```snapshot
warning[redundant-condition]: Function `make_coroutine` is always truthy
  --> src/mdtest_snippet.py:58:8
   |
58 |     if make_coroutine:  # snapshot: redundant-condition
   |        ^^^^^^^^^^^^^^ Did you mean to `await` and call this function?
   |
57 | async def check_coroutine_factory():
   -     if make_coroutine:  # snapshot: redundant-condition
58 +     if await make_coroutine():  # snapshot: redundant-condition
59 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

An awaitable in a synchronous function or a lambda still produces a diagnostic, but suggesting
`await` would create invalid syntax, so we also do not add an autofix here:

```py
def inspect_synchronous_awaitable():
    if coroutine():  # snapshot: redundant-condition
        pass

async def inspect_lambda_awaitable():
    return lambda: True if coroutine() else False  # snapshot: redundant-condition
```

```snapshot
warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:65:8
   |
65 |     if coroutine():  # snapshot: redundant-condition
   |        ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:69:28
   |
69 |     return lambda: True if coroutine() else False  # snapshot: redundant-condition
   |                            ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
```

Awaiting an expression is valid within a comprehension in an asynchronous function or within a
generator expression:

```py
async def inspect_comprehension_awaitable():
    return [value for value in range(1) if coroutine()]  # snapshot: redundant-condition

def inspect_generator_awaitable():
    return (value for value in range(1) if coroutine())  # snapshot: redundant-condition
```

```snapshot
warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:71:44
   |
71 |     return [value for value in range(1) if coroutine()]  # snapshot: redundant-condition
   |                                            ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
   |
70 | async def inspect_comprehension_awaitable():
   -     return [value for value in range(1) if coroutine()]  # snapshot: redundant-condition
71 +     return [value for value in range(1) if await coroutine()]  # snapshot: redundant-condition
72 |
   |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:74:44
   |
74 |     return (value for value in range(1) if coroutine())  # snapshot: redundant-condition
   |                                            ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
   |
73 | def inspect_generator_awaitable():
   -     return (value for value in range(1) if coroutine())  # snapshot: redundant-condition
74 +     return (value for value in range(1) if await coroutine())  # snapshot: redundant-condition
75 | async def inspect_named_awaitable():
   |
note: This is an unsafe fix and may change runtime behavior
```

Assignment expressions need parentheses so the assignment still happens before awaiting its result:

```py
async def inspect_named_awaitable():
    if value := coroutine():  # snapshot: redundant-condition-strict
        pass
```

```snapshot
error[redundant-condition-strict]: Condition is always truthy
  --> src/mdtest_snippet.py:76:8
   |
76 |     if value := coroutine():  # snapshot: redundant-condition-strict
   |        ^^^^^^^^^^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
   |
75 | async def inspect_named_awaitable():
   -     if value := coroutine():  # snapshot: redundant-condition-strict
76 +     if await (value := coroutine()):  # snapshot: redundant-condition-strict
77 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

Unary and binary operations need parentheses so the entire original expression is awaited:

```py
class AwaitableOperations:
    async def __neg__(self) -> bool:
        return True

    async def __add__(self, other: object) -> bool:
        return True

async def inspect_awaitable_operations(value: AwaitableOperations):
    if -value:  # snapshot: redundant-condition
        pass

    if value + value:  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:86:8
   |
86 |     if -value:  # snapshot: redundant-condition
   |        ^^^^^^ Inferred type is `CoroutineType[Any, Any, bool]`
help: Did you mean to `await` this expression?
   |
85 | async def inspect_awaitable_operations(value: AwaitableOperations):
   -     if -value:  # snapshot: redundant-condition
86 +     if await (-value):  # snapshot: redundant-condition
87 |         pass
   |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:89:8
   |
89 |     if value + value:  # snapshot: redundant-condition
   |        ^^^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, bool]`
help: Did you mean to `await` this expression?
   |
88 |
   -     if value + value:  # snapshot: redundant-condition
89 +     if await (value + value):  # snapshot: redundant-condition
90 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

Conditional expressions also need parentheses so the selected branch is awaited:

```py
async def inspect_conditional_awaitable(flag: bool):
    if coroutine() if flag else coroutine():  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:92:8
   |
92 |     if coroutine() if flag else coroutine():  # snapshot: redundant-condition
   |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
   |
91 | async def inspect_conditional_awaitable(flag: bool):
   -     if coroutine() if flag else coroutine():  # snapshot: redundant-condition
92 +     if await (coroutine() if flag else coroutine()):  # snapshot: redundant-condition
93 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

An expression that has already been awaited needs parentheses before adding another `await`:

```py
async def nested_coroutine() -> CoroutineType[Any, Any, bool]:
    return coroutine()

async def inspect_nested_awaitable():
    if await nested_coroutine():  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Condition is always truthy
  --> src/mdtest_snippet.py:98:8
   |
98 |     if await nested_coroutine():  # snapshot: redundant-condition
   |        ^^^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, bool]`
help: Did you mean to `await` this expression?
   |
97 | async def inspect_nested_awaitable():
   -     if await nested_coroutine():  # snapshot: redundant-condition
98 +     if await (await nested_coroutine()):  # snapshot: redundant-condition
99 |         pass
   |
note: This is an unsafe fix and may change runtime behavior
```

Annotations, type aliases, type-parameter bounds, and generic class bases cannot contain `await`,
even when they appear inside an asynchronous function. Their diagnostics therefore have no autofix:

```py
from typing import Annotated

class Base: ...

async def inspect_restricted_awaitable_contexts():
    type Alias = Annotated[int, 1 if coroutine() else 0]  # snapshot: redundant-condition

    class Generic[T: Annotated[int, 1 if coroutine() else 0]]:  # snapshot: redundant-condition
        pass

    def generic[T: Annotated[int, 1 if coroutine() else 0]]():  # snapshot: redundant-condition
        pass

    type GenericAlias[T: Annotated[int, 1 if coroutine() else 0]] = list[T]  # snapshot: redundant-condition

    class GenericBase[T](Base if coroutine() else Base):  # snapshot: redundant-condition
        pass

    def nested(value: Annotated[int, 1 if coroutine() else 0]):  # snapshot: redundant-condition
        pass

    def returned() -> Annotated[int, 1 if coroutine() else 0]:  # snapshot: redundant-condition
        return 1

    variable: Annotated[int, 1 if coroutine() else 0]  # snapshot: redundant-condition

    list_comprehension: Annotated[int, [value for value in range(1) if coroutine()]]  # snapshot: redundant-condition
    set_comprehension: Annotated[int, {value for value in range(1) if coroutine()}]  # snapshot: redundant-condition
    dict_comprehension: Annotated[int, {value: value for value in range(1) if coroutine()}]  # snapshot: redundant-condition

    def nested_comprehension(
        value: Annotated[int, [item for item in range(1) if coroutine()]],  # snapshot: redundant-condition
    ):
        pass

    def returned_comprehension() -> Annotated[
        int, [value for value in range(1) if coroutine()]  # snapshot: redundant-condition
    ]:
        return 1

class AnnotatedHolder:
    async def inspect(self):
        self.value: Annotated[int, 1 if coroutine() else 0]  # snapshot: redundant-condition
```

```snapshot
warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:105:38
    |
105 |     type Alias = Annotated[int, 1 if coroutine() else 0]  # snapshot: redundant-condition
    |                                      ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:107:42
    |
107 |     class Generic[T: Annotated[int, 1 if coroutine() else 0]]:  # snapshot: redundant-condition
    |                                          ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:110:40
    |
110 |     def generic[T: Annotated[int, 1 if coroutine() else 0]]():  # snapshot: redundant-condition
    |                                        ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:113:46
    |
113 |     type GenericAlias[T: Annotated[int, 1 if coroutine() else 0]] = list[T]  # snapshot: redundant-condition
    |                                              ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:115:34
    |
115 |     class GenericBase[T](Base if coroutine() else Base):  # snapshot: redundant-condition
    |                                  ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:118:43
    |
118 |     def nested(value: Annotated[int, 1 if coroutine() else 0]):  # snapshot: redundant-condition
    |                                           ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:121:43
    |
121 |     def returned() -> Annotated[int, 1 if coroutine() else 0]:  # snapshot: redundant-condition
    |                                           ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:124:35
    |
124 |     variable: Annotated[int, 1 if coroutine() else 0]  # snapshot: redundant-condition
    |                                   ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:126:72
    |
126 |     list_comprehension: Annotated[int, [value for value in range(1) if coroutine()]]  # snapshot: redundant-condition
    |                                                                        ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:127:71
    |
127 |     set_comprehension: Annotated[int, {value for value in range(1) if coroutine()}]  # snapshot: redundant-condition
    |                                                                       ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:128:79
    |
128 |     dict_comprehension: Annotated[int, {value: value for value in range(1) if coroutine()}]  # snapshot: redundant-condition
    |                                                                               ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:131:61
    |
131 |         value: Annotated[int, [item for item in range(1) if coroutine()]],  # snapshot: redundant-condition
    |                                                             ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:136:46
    |
136 |         int, [value for value in range(1) if coroutine()]  # snapshot: redundant-condition
    |                                              ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:142:41
    |
142 |         self.value: Annotated[int, 1 if coroutine() else 0]  # snapshot: redundant-condition
    |                                         ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
```

A generator expression introduces a scope where `await` is valid even when the generator appears
inside an annotation. This also permits awaiting inside a comprehension nested in that generator:

```py
async def inspect_generator_annotations():
    direct: Annotated[int, (value for value in range(1) if coroutine())]  # snapshot: redundant-condition
    nested: Annotated[int, ([value for value in range(1) if coroutine()] for _ in range(1))]  # snapshot: redundant-condition
```

```snapshot
warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:144:60
    |
144 |     direct: Annotated[int, (value for value in range(1) if coroutine())]  # snapshot: redundant-condition
    |                                                            ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
    |
143 | async def inspect_generator_annotations():
    -     direct: Annotated[int, (value for value in range(1) if coroutine())]  # snapshot: redundant-condition
144 +     direct: Annotated[int, (value for value in range(1) if await coroutine())]  # snapshot: redundant-condition
145 |     nested: Annotated[int, ([value for value in range(1) if coroutine()] for _ in range(1))]  # snapshot: redundant-condition
    |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:145:61
    |
145 |     nested: Annotated[int, ([value for value in range(1) if coroutine()] for _ in range(1))]  # snapshot: redundant-condition
    |                                                             ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
    |
144 |     direct: Annotated[int, (value for value in range(1) if coroutine())]  # snapshot: redundant-condition
    -     nested: Annotated[int, ([value for value in range(1) if coroutine()] for _ in range(1))]  # snapshot: redundant-condition
145 +     nested: Annotated[int, ([value for value in range(1) if await coroutine()] for _ in range(1))]  # snapshot: redundant-condition
146 | async def inspect_allowed_definition_awaitables():
    |
note: This is an unsafe fix and may change runtime behavior
```

Non-generic class bases and function parameter defaults can contain `await` when they are evaluated
in an asynchronous function, even if the function being defined has type parameters:

```py
async def inspect_allowed_definition_awaitables():
    class NongenericBase(Base if coroutine() else Base):  # snapshot: redundant-condition
        pass

    def generic_default[T](value: int = 1 if coroutine() else 0):  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:147:34
    |
147 |     class NongenericBase(Base if coroutine() else Base):  # snapshot: redundant-condition
    |                                  ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
    |
146 | async def inspect_allowed_definition_awaitables():
    -     class NongenericBase(Base if coroutine() else Base):  # snapshot: redundant-condition
147 +     class NongenericBase(Base if await coroutine() else Base):  # snapshot: redundant-condition
148 |         pass
    |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:150:46
    |
150 |     def generic_default[T](value: int = 1 if coroutine() else 0):  # snapshot: redundant-condition
    |                                              ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
    |
149 |
    -     def generic_default[T](value: int = 1 if coroutine() else 0):  # snapshot: redundant-condition
150 +     def generic_default[T](value: int = 1 if await coroutine() else 0):  # snapshot: redundant-condition
151 |         pass
    |
note: This is an unsafe fix and may change runtime behavior
```

Type expressions used as runtime values and the values of annotated assignments are ordinary Python
expressions, so they can contain `await` inside an asynchronous function:

```py
async def inspect_runtime_type_expressions():
    alias = list[Annotated[int, 1 if coroutine() else 0]]  # snapshot: redundant-condition
    value: int = 1 if coroutine() else 0  # snapshot: redundant-condition
```

```snapshot
warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:153:38
    |
153 |     alias = list[Annotated[int, 1 if coroutine() else 0]]  # snapshot: redundant-condition
    |                                      ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
    |
152 | async def inspect_runtime_type_expressions():
    -     alias = list[Annotated[int, 1 if coroutine() else 0]]  # snapshot: redundant-condition
153 +     alias = list[Annotated[int, 1 if await coroutine() else 0]]  # snapshot: redundant-condition
154 |     value: int = 1 if coroutine() else 0  # snapshot: redundant-condition
    |
note: This is an unsafe fix and may change runtime behavior


warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:154:23
    |
154 |     value: int = 1 if coroutine() else 0  # snapshot: redundant-condition
    |                       ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
    |
153 |     alias = list[Annotated[int, 1 if coroutine() else 0]]  # snapshot: redundant-condition
    -     value: int = 1 if coroutine() else 0  # snapshot: redundant-condition
154 +     value: int = 1 if await coroutine() else 0  # snapshot: redundant-condition
155 | async def inspect_compound_awaitable(flag: bool):
    |
note: This is an unsafe fix and may change runtime behavior
```

An awaitable in the final operand of a compound condition still receives an autofix when the
condition as a whole has ambiguous truthiness:

```py
async def inspect_compound_awaitable(flag: bool):
    if flag and coroutine():  # snapshot: redundant-condition
        pass
```

```snapshot
warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:156:17
    |
156 |     if flag and coroutine():  # snapshot: redundant-condition
    |                 ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
help: Did you mean to `await` this expression?
    |
155 | async def inspect_compound_awaitable(flag: bool):
    -     if flag and coroutine():  # snapshot: redundant-condition
156 +     if flag and await coroutine():  # snapshot: redundant-condition
157 |         pass
    |
note: This is an unsafe fix and may change runtime behavior
```

Python modules do not allow top-level `await`, so awaitable conditions at module scope have no
autofix:

```py
if coroutine():  # snapshot: redundant-condition
    pass
```

```snapshot
warning[redundant-condition]: Condition is always truthy
   --> src/mdtest_snippet.py:158:4
    |
158 | if coroutine():  # snapshot: redundant-condition
    |    ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, Unknown]`
```

## Notebook cells

Notebook cells do allow top-level `await`, so the same condition receives an autofix there:

```ipynb
{
  "cells": [
    {
      "cell_type": "code",
      "execution_count": null,
      "metadata": {},
      "outputs": [],
      "source": [
        "async def coroutine() -> bool:\n",
        "    return False\n",
        "\n",
        "if coroutine():  # snapshot: redundant-condition\n",
        "    pass\n"
      ]
    }
  ],
  "metadata": {},
  "nbformat": 4,
  "nbformat_minor": 4
}
```

```snapshot
warning[redundant-condition]: Condition is always truthy
 --> src/mdtest_snippet.ipynb:cell 1:4:4
  |
4 | if coroutine():  # snapshot: redundant-condition
  |    ^^^^^^^^^^^ Inferred type is `CoroutineType[Any, Any, bool]`
help: Did you mean to `await` this expression?
 ::: cell 1
  |
3 |
  - if coroutine():  # snapshot: redundant-condition
4 + if await coroutine():  # snapshot: redundant-condition
5 |     pass
  |
note: This is an unsafe fix and may change runtime behavior
```

## Strict version

Our stricter `redundant-condition-strict` rule extends this logic to boolean and integer tests:

```py
from typing import Literal

def f(x: Literal[1, 2]):
    if x > 5:  # error: [redundant-condition-strict]
        pass

    if x:  # snapshot: redundant-condition-strict
        pass

def g(flag: bool, some_bytes: bytes):
    if flag:
        pass
    elif some_bytes[0] == b"\x1e":  # snapshot: redundant-condition-strict
        pass

def falsy(flag: bool):
    if flag:
        pass
    elif "foo" == b"foo":  # snapshot: redundant-condition-strict
        pass
```

```snapshot
error[redundant-condition-strict]: Condition is always truthy
 --> src/mdtest_snippet.py:7:8
  |
7 |     if x:  # snapshot: redundant-condition-strict
  |        ^ Inferred type is `Literal[1, 2]`


error[redundant-condition-strict]: Condition is always false
  --> src/mdtest_snippet.py:13:10
   |
13 |     elif some_bytes[0] == b"/x1e":  # snapshot: redundant-condition-strict
   |          -------------^^^^-------
   |          |                |
   |          |                Has type `Literal[b"/x1e"]`
   |          Has type `int`


error[redundant-condition-strict]: Condition is always false
  --> src/mdtest_snippet.py:19:10
   |
19 |     elif "foo" == b"foo":  # snapshot: redundant-condition-strict
   |          -----^^^^------
   |          |        |
   |          |        Has type `Literal[b"foo"]`
   |          Has type `Literal["foo"]`
```

`redundant-condition-strict` is also emitted on negated conditions where the negated condition is
inferred as an instance of `bool`:

```py
def negated_conditions():
    if not 1 > 2:  # error: [redundant-condition-strict] "Condition `not 1 > 2` is always true"
        pass

    if not 1 < 2:  # error: [redundant-condition-strict] "Condition `not 1 < 2` is always false"
        pass

    if not 0 == 1:  # error: [redundant-condition-strict] "Condition `not 0 == 1` is always true"
        pass

    if not 1 == 1:  # error: [redundant-condition-strict] "Condition `not 1 == 1` is always false"
        pass

    if not not 1 == 1:  # error: [redundant-condition-strict] "Condition `not not 1 == 1` is always true"
        pass

def negated_conditional_contexts(flag: bool):
    if flag:
        pass
    elif not 1 == 0:  # error: [redundant-condition-strict] "Condition `not 1 == 0` is always true"
        pass

    while not 1 == 0:  # error: [redundant-condition-strict] "Condition `not 1 == 0` is always true"
        break
```

Outside a statement condition, a `not` expression still tests its operand's truthiness. The strict
rule reports redundant boolean and integer operands in assignments and return expressions:

```py
def negated_boolean_assignment(value: str):
    result = not isinstance(value, str)  # error: [redundant-condition-strict] "Condition `isinstance(value, str)` is always true"

def negated_integer_return(value: Literal[1, 2]) -> bool:
    return not value  # error: [redundant-condition-strict] "Object of type `Literal[1, 2]` is always truthy"
```

To avoid two diagnostics being emitted on compound tests such as the following statements, we
suppress `redundant-condition-strict` on subexpressions of `if`-statement tests, `elif` tests and
`while` tests. Only a single diagnostic is emitted on each of these:

```py
def compound_truthy(x: str):
    if isinstance(x, str) and isinstance(x, str):  # error: [redundant-condition-strict]
        pass

    while isinstance(x, str) and isinstance(x, str):  # error: [redundant-condition-strict]
        break

    match x:
        case str() if isinstance(x, str) and isinstance(x, str):  # error: [redundant-condition-strict]
            pass
```

## Replacing a redundant final `elif` with an assertion

When a final `elif` condition is always true, an `else` branch containing the same condition as a
defensive assertion makes the exhaustiveness check explicit without repeating the condition as a
branch:

```py
def exhaustive(value: str | int):
    if isinstance(value, str):
        print(value)
    elif isinstance(value, int):  # snapshot: redundant-condition-strict
        print(value)
        print(value + 1)
```

```snapshot
error[redundant-condition-strict]: Condition is always true
 --> src/mdtest_snippet.py:4:10
  |
4 |     elif isinstance(value, int):  # snapshot: redundant-condition-strict
  |          ^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `Literal[True]`
help: Replace this `elif` with an `else` branch that asserts the condition to be `True`
  |
3 |         print(value)
  -     elif isinstance(value, int):  # snapshot: redundant-condition-strict
4 +     else:  # snapshot: redundant-condition-strict
5 +         assert isinstance(value, int)
6 |         print(value)
  |
note: This is an unsafe fix and may change runtime behavior
```

The assertion uses the existing indentation of the branch body, including unconventional
indentation:

```py
# fmt: off
def unconventional_indentation(value: str | int):
  if isinstance(value, str):
    print(value)
  elif isinstance(value, int):  # snapshot: redundant-condition-strict
    print(value)
# fmt: on
```

```snapshot
error[redundant-condition-strict]: Condition is always true
  --> src/mdtest_snippet.py:11:8
   |
11 |   elif isinstance(value, int):  # snapshot: redundant-condition-strict
   |        ^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `Literal[True]`
help: Replace this `elif` with an `else` branch that asserts the condition to be `True`
   |
10 |     print(value)
   -   elif isinstance(value, int):  # snapshot: redundant-condition-strict
11 +   else:  # snapshot: redundant-condition-strict
12 +     assert isinstance(value, int)
13 |     print(value)
   |
note: This is an unsafe fix and may change runtime behavior
```

Comments inside a parenthesized condition, after the branch header, and before its first statement
are all preserved:

```py
def commented_condition(value: str | int):
    if isinstance(value, str):
        print(value)
    elif (
        # Explain the defensive runtime check.
        isinstance(value, int)  # snapshot: redundant-condition-strict
    ):  # Preserve this header comment.
        # Preserve this body comment.
        print(value)
```

```snapshot
error[redundant-condition-strict]: Condition is always true
  --> src/mdtest_snippet.py:19:9
   |
19 |         isinstance(value, int)  # snapshot: redundant-condition-strict
   |         ^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `Literal[True]`
help: Replace this `elif` with an `else` branch that asserts the condition to be `True`
   |
16 |         print(value)
   -     elif (
17 +     else:  # Preserve this header comment.
18 +         # Preserve this body comment.
19 +         assert (
20 |         # Explain the defensive runtime check.
21 |         isinstance(value, int)  # snapshot: redundant-condition-strict
   -     ):  # Preserve this header comment.
   -         # Preserve this body comment.
22 +     )
23 |         print(value)
   |
note: This is an unsafe fix and may change runtime behavior
```

An unparenthesized assignment expression is valid in an `elif` condition but must be parenthesized
when moved into an assertion:

```py
def assignment_expression(value: str | int):
    if isinstance(value, str):
        print(value)
    elif matched := isinstance(value, int):  # snapshot: redundant-condition-strict
        print(matched)
```

```snapshot
error[redundant-condition-strict]: Condition is always true
  --> src/mdtest_snippet.py:26:10
   |
26 |     elif matched := isinstance(value, int):  # snapshot: redundant-condition-strict
   |          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `Literal[True]`
help: Replace this `elif` with an `else` branch that asserts the condition to be `True`
   |
25 |         print(value)
   -     elif matched := isinstance(value, int):  # snapshot: redundant-condition-strict
26 +     else:  # snapshot: redundant-condition-strict
27 +         assert (matched := isinstance(value, int))
28 |         print(matched)
   |
note: This is an unsafe fix and may change runtime behavior
```

If the branch body begins on the same line as its header, inserting a separate assertion would
require rewriting the body, so no autofix is offered:

```py
# fmt: off
def inline_branch(value: str | int):
    if isinstance(value, str):
        print(value)
    elif isinstance(value, int): print(value)  # snapshot: redundant-condition-strict
# fmt: on
```

```snapshot
error[redundant-condition-strict]: Condition is always true
  --> src/mdtest_snippet.py:32:10
   |
32 |     elif isinstance(value, int): print(value)  # snapshot: redundant-condition-strict
   |          ^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `Literal[True]`
help: Replace this `elif` with an `else` branch that asserts the condition to be `True`
```

A multiline header also receives no autofix when its body begins on the same line as its closing
colon:

```py
# fmt: off
def multiline_inline_branch(value: str | int):
    if isinstance(value, str):
        print(value)
    elif (
        isinstance(value, int)  # snapshot: redundant-condition-strict
    ): print(value)
# fmt: on
```

```snapshot
error[redundant-condition-strict]: Condition is always true
  --> src/mdtest_snippet.py:39:9
   |
39 |         isinstance(value, int)  # snapshot: redundant-condition-strict
   |         ^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `Literal[True]`
help: Replace this `elif` with an `else` branch that asserts the condition to be `True`
```

Parser recovery can produce an `elif` branch with no statements. The redundant condition is still
reported, but no autofix is offered for the incomplete branch:

```py
def empty_branch(value: str | int):
    if isinstance(value, str):
        print(value)
    # error: [invalid-syntax] "Expected an indented block after `elif` clause"
    elif isinstance(value, int):  # snapshot: redundant-condition-strict
```

```snapshot
error[redundant-condition-strict]: Condition is always true
  --> src/mdtest_snippet.py:46:10
   |
46 |     elif isinstance(value, int):  # snapshot: redundant-condition-strict
   |          ^^^^^^^^^^^^^^^^^^^^^^ Inferred type is `Literal[True]`
help: Replace this `elif` with an `else` branch that asserts the condition to be `True`
```

A boolean `elif` following a non-boolean condition still receives the assertion fix. The type of the
first condition does not determine how later conditions are diagnosed.

```py
def non_boolean_first_condition(items: list[int], value: int):
    if items:
        print(items)
    elif value is not None:  # snapshot: redundant-condition-strict
        print(value)
```

```snapshot
error[redundant-condition-strict]: Condition is always true
  --> src/mdtest_snippet.py:50:10
   |
50 |     elif value is not None:  # snapshot: redundant-condition-strict
   |          -----^^^^^^^^----
   |          |            |
   |          |            Has type `None`
   |          Has type `int`
help: Replace this `elif` with an `else` branch that asserts the condition to be `True`
   |
49 |         print(items)
   -     elif value is not None:  # snapshot: redundant-condition-strict
50 +     else:  # snapshot: redundant-condition-strict
51 +         assert value is not None
52 |         print(value)
   |
note: This is an unsafe fix and may change runtime behavior
```

## Infinite `while` loops

We maintain a special case for `while` loops, since `while True:` and `while 1:` are common idioms
used to create infinite loops in Python code. Complaining that the conditions `True` and `1` are
"always truthy" in these contexts would obviously be absurd.

Note that these need to be tested in separate files, as ty infers all code after a `while True` or
`while 1` loop to be unreachable, and it does not emit any diagnostics in unreachable code!

`while_true.py`:

```py
while True:  # no error
    pass
```

`while_1.py`:

```py
while 1:
    pass  # no error
```

## `if` conditions that use AST literal bools or ints

Some projects use literal `if False:` or `if 0:` in their source code, to mark a region that is
intentionally unreachable, but which could be enabled for debugging purposes. If we see an AST
literal used as a condition, rather than a place that is inferred as having a literal *type*, we
suppress the diagnostic: it is assumed that this region is deliberately unreachable.

```py
if False:  # no diagnostic
    pass

if 0:  # no diagnostic
    pass
```

For consistency, we do the same for `if True:`, `if 1:`, `if 2:`, etc.:

```py
if 1:  # no diagnostic
    pass

if True:  # no diagnostic
    pass

if 2:  # no diagnostic
    pass
```

## Defensive assertions

Of the two rules, only `redundant-condition` is applied to tests in `assert` statements (and any
subexpressions within those tests). This is to prevent false positives on defensive assertions such
as the following, which are common in well written Python code:

```py
def f(x: str, y: str | int, z: str | int | bytes):
    assert isinstance(x, str)
    assert isinstance(y, str) or isinstance(y, int)
    assert isinstance(z, str) or isinstance(z, int) or isinstance(z, bytes)
    assert isinstance(x, str) and isinstance(y, (str, int))
    assert not not isinstance(x, str)
    assert isinstance(x, str) and (isinstance(y, str) or isinstance(y, int))
    assert (isinstance(y, str) or isinstance(y, int)) and not not isinstance(x, str)
```

The ordinary rule still applies inside assertion tests, and the strict rule still applies to
assertion messages:

```py
def func(): ...
def assertion_boundaries(x: str, flag: bool):
    assert func and isinstance(x, str)  # error: [redundant-condition]
    assert flag, isinstance(x, str) and flag  # error: [redundant-condition-strict]
```

## `sys.version_info` checks, `sys.platform` checks, `os.name` checks, `if TYPE_CHECKING` checks

Certain stdlib constants are heavily special-cased by ty, leading us to infer that certain `if`
tests involving these constants will always be truthy or always be falsy. Since the branches of code
here are deliberately unreachable, we try to avoid emitting false-positive diagnostics on these as
well:

`a.py`:

```py
import sys
import os
import typing
from typing import TYPE_CHECKING

def coinflip() -> bool:
    return False

reveal_type(sys.version_info >= (3, 14))  # revealed: Literal[True]
reveal_type(sys.version_info < (3, 15))  # revealed: Literal[True]

if sys.version_info >= (3, 14):  # no diagnostic
    pass

if coinflip():
    pass
elif sys.version_info < (3, 15):  # no diagnostic
    pass

if os.name == "posix":  # no diagnostic
    pass

if coinflip():
    pass
elif os.name == "nt":  # no diagnostic
    pass

reveal_type(TYPE_CHECKING)  # revealed: Literal[True]

if TYPE_CHECKING:  # no diagnostic
    pass

reveal_type(typing.TYPE_CHECKING)  # revealed: Literal[True]

if not typing.TYPE_CHECKING:  # no diagnostic
    pass

if sys.version_info < (3, 15):
    pass
elif (3, 12) <= sys.version_info < (3, 13):  # no diagnostic
    pass

if os.name == "posix":
    pass
elif os.name == "nt":  # no diagnostic
    pass
```

This also applies to the enabled-by-default `redundant-condition` rule, which only applies when
checking a condition that is not inferred as being assignable to `int`:

`b.py`:

```py
import sys

catch_exe_failure = "\n" if sys.platform == "win32" else ""

reveal_type(catch_exe_failure)  # revealed: Literal[""]

# This
if catch_exe_failure:  # no diagnostic
    pass
```

This even applies to cases where the value of one of these constants is aliased to a variable in the
module namespace:

`c.py`:

```py
import os
import sys
from os import name as os_name
from typing import TYPE_CHECKING
from typing_extensions import TYPE_CHECKING as TYPE_CHECKINGGGGG
from sys import version_info as foo, platform as sys_platform

PLATFORM = sys.platform

if PLATFORM == "linux":  # no diagnostic
    pass

PLATFORM_ALIAS = PLATFORM

if PLATFORM_ALIAS == "linux":  # no diagnostic
    pass

OS_MODULE = os
OPERATING_SYSTEM = OS_MODULE.name

if OPERATING_SYSTEM == "posix":  # no diagnostic
    pass

IS_PY314 = sys.version_info >= (3, 14)
reveal_type(IS_PY314)  # revealed: Literal[True]

if IS_PY314:  # no diagnostic
    pass

if not IS_PY314:  # no diagnostic
    pass

VERSION_INFO = sys.version_info

if VERSION_INFO >= (3, 14):  # no diagnostic
    pass

CHECKING = TYPE_CHECKING

if CHECKING:  # no diagnostic
    pass

ORDINARY_CONSTANT = 1 == 1

if ORDINARY_CONSTANT:  # error: [redundant-condition-strict]
    pass

BAR = foo

reveal_type(BAR >= (3, 14))  # revealed: Literal[True]

if BAR >= (3, 14):  # no diagnostic
    pass

reveal_type(TYPE_CHECKINGGGGG)  # revealed: Literal[True]

if TYPE_CHECKINGGGGG:
    pass

reveal_type(sys_platform)  # revealed: Literal["linux"]

if sys_platform == "linux":  # no diagnostic
    pass

reveal_type(os_name)  # revealed: Literal["posix"]

if os_name == "posix":  # no diagnostic
    pass
```

And even in other imported modules:

`d.py`:

```py
import c
from c import IS_PY314, PLATFORM, BAR

if PLATFORM == "linux":  # no diagnostic
    pass

if c.PLATFORM_ALIAS == "linux":  # no diagnostic
    pass

if IS_PY314:  # no diagnostic
    pass

reveal_type(BAR >= (3, 14))  # revealed: Literal[True]

if BAR >= (3, 14):  # no diagnostic
    pass
```

Attribute aliases retain their environment-dependent origin. Different members of the same receiver
can have different origins, and rebinding or narrowing the receiver can change which definition an
attribute refers to.

`attribute_aliases.py`:

```py
import sys
from typing import Final

class PlatformConfig:
    enabled: Final = sys.platform == "linux"
    fixed: Final = True

class FixedConfig:
    enabled: Final = True

def rebound_receiver():
    config = PlatformConfig()
    if config.enabled:
        pass
    if config.fixed:  # error: [redundant-condition-strict] "Condition `config.fixed` is always true"
        pass

    config = FixedConfig()
    if config.enabled:  # error: [redundant-condition-strict] "Condition `config.enabled` is always true"
        pass

def narrowed_receiver(config: PlatformConfig | FixedConfig):
    if config.enabled:
        pass

    if isinstance(config, FixedConfig):
        if config.enabled:  # error: [redundant-condition-strict] "Condition `config.enabled` is always true"
            pass
    else:
        if config.enabled:
            pass
```

Named expressions and unpacked assignments preserve the same environment-dependent origin as
ordinary assignments. Their aliases remain exempt when tested later.

`assignment_forms.py`:

```py
import sys

if windows := sys.platform == "win32":
    pass
if windows:
    pass

unix, version = sys.platform != "win32", sys.version_info
if unix:
    pass
if version >= (3, 14):
    pass

def local_aliases():
    if is_windows := sys.platform == "win32":
        pass
    if is_windows:
        pass

    is_unix, major = sys.platform != "win32", sys.version_info.major
    if is_unix:
        pass
    if major >= 3:
        pass

if ordinary := 1 == 1:  # error: [redundant-condition-strict] "Condition `ordinary := 1 == 1` is always true"
    pass
if ordinary:  # error: [redundant-condition-strict] "Condition `ordinary` is always true"
    pass
```

Following aliases also terminates when assignments form a cycle. An ordinary cycle does not make an
always-truthy condition environment-dependent, whether the aliases are names or instance attributes.

`cyclic_aliases.py`:

```py
def plain_cycle(flag: bool):
    first = second = "ready"
    while flag:
        first = second
        second = first
    if first:  # error: [redundant-condition] "Object of type `Literal["ready"]` is always truthy"
        pass

class AttributeCycle:
    def check(self, flag: bool):
        self.first = self.second = "ready"
        while flag:
            self.first = self.second
            self.second = self.first
        if self.first:  # error: [redundant-condition] "Object of type `Literal["ready"]` is always truthy"
            pass
```

An environment-dependent assignment is still recognized after following a cycle of
instance-attribute aliases.

```py
import sys

class PlatformAttributeCycle:
    def check(self, flag: bool):
        self.first = self.second = "ready"
        while flag:
            self.first = self.second
            self.second = self.first
            self.second = sys.platform
        reveal_type(bool(self.first))  # revealed: Literal[True]
        if self.first:
            pass
```

## Deliberately exhaustive `if` statements

A common pattern is to have an `if` condition that is deliberately always true or false, so that the
user can assert exhaustiveness explicitly. We detect these cases and avoid emitting diagnostics on
them.

```py
import sys
from typing_extensions import assert_never

def f1(x: int | str):
    if isinstance(x, int):
        pass
    # always True, but no diagnostic emitted: the `else` block following only contains `raise` statements
    elif isinstance(x, str):
        pass
    else:
        raise AssertionError
        
def f2(x: int | str):
    if isinstance(x, int):
        pass
    # always False, but no diagnostic emitted: the block only contains `raise` statements
    elif not isinstance(x, str):
        raise AssertionError

def f3(x: int | str):
    if isinstance(x, int):
        pass
    # always True, but no diagnostic emitted: the `else` block following only contains `assert` statements
    elif isinstance(x, str):
        pass
    else:
        assert False, "unreachable"

def f4(x: int | str):
    if isinstance(x, int):
        pass
    # always True, but no diagnostic emitted: the `else` block following only contains calls that return `Never`
    elif isinstance(x, str):
        pass
    else:
        assert_never(x)

def f5(x: int | str):
    if isinstance(x, int):
        pass
    # always True, but no diagnostic emitted: the `else` block following only contains calls that return `Never`
    elif isinstance(x, str):
        pass
    else:
        "Some documentation as a standalone string, weirdly"
        sys.exit("This should never happen??")

def f6(x: int):
    # always True, but no diagnostic emitted: the block inside the `if` only contains `raise` statements
    if not isinstance(x, int):
        raise TypeError

def f7(x: int | str):
    if isinstance(x, int):
        pass
    # always True, but no diagnostic emitted: the `else` block following only contains `raise` statements
    elif isinstance(x, str) and not isinstance(x, int):
        pass
    else:
        raise AssertionError

def f8(x: int | str):
    if isinstance(x, int):
        pass
    # always False, but no diagnostic emitted: the block only contains `raise` statements
    elif not isinstance(x, str) or isinstance(x, int):
        raise AssertionError

def f9(x: str):
    # always False, but no diagnostic emitted: the block only contains `raise` statements
    if isinstance(x, str) and not isinstance(x, str):
        raise AssertionError

def f10(x: str):
    # always False, but no diagnostic emitted: the block only contains `raise` statements
    if not (isinstance(x, str) and isinstance(x, str)):
        raise TypeError

def coinflip() -> bool:
    return True

def f11(x: str):
    # always True, but no diagnostic emitted: every control flow path can be easily determined
    # to end in a terminal statement
    if not isinstance(x, str):
        if coinflip():
            message = "seems bad"
            raise TypeError(message)
        else:
            assert False, "oh no"
```

We also avoid emitting the diagnostic if the exhaustiveness check just follows the if check, and is
not in an `else` branch:

```py
def g(x: int | str):
    if isinstance(x, int):
        return

    # always True, but no diagnostic emitted: the code following only contains `raise` statements
    if isinstance(x, str):
        return

    raise AssertionError

def g2(x: int | str):
    if isinstance(x, int):
        return
    # always True, but no diagnostic emitted: the code following only contains `assert` statements
    elif isinstance(x, str):
        return
    
    assert False, "unreachable"
```

This also works if the entire block is nested:

```py
def unrelated_condition() -> bool:
    return False

def h(x: int | str):
    if unrelated_condition():
        if isinstance(x, int):
            return

        # always True, but no diagnostic emitted: the code following only contains `raise` statements
        if isinstance(x, str):
            return

        raise AssertionError
    # do other things that aren't raises or assertions:
    x = 1
```

An assertion that always succeeds does not establish exhaustiveness, whether it appears in the
conditional body, an `else` block, or immediately after the statement:

```py
def successful_assertion_in_body(value: int):
    if value is None:  # error: [redundant-condition-strict] "Condition `value is None` is always false"
        assert True

def successful_assertion_in_else(value: int):
    if value is not None:  # error: [redundant-condition-strict] "Condition `value is not None` is always true"
        pass
    else:
        assert True

def successful_assertion_after_if(value: int):
    if value is not None:  # error: [redundant-condition-strict] "Condition `value is not None` is always true"
        pass
    assert True
```

A nested conditional is only a defensive exit if its initial `if` body and every `elif` and `else`
body end in defensive exits. A body that falls through does not establish exhaustiveness.

```py
def nested_fallthrough(value: int, flag: bool):
    if value is None:  # error: [redundant-condition-strict] "Condition `value is None` is always false"
        if flag:
            print(value)
        else:
            raise AssertionError

def nested_without_else(value: int, flag: bool):
    if value is None:  # error: [redundant-condition-strict] "Condition `value is None` is always false"
        if flag:
            raise AssertionError
```

The first condition's type does not affect whether a later boolean condition is recognized as a
defensive check. Non-boolean conditions still produce the ordinary diagnostic, even when followed by
a defensive exit and the strict rule is enabled.

```py
def defensive_elif(items: list[int], value: int):
    if items:
        pass
    elif value is None:
        raise AssertionError

def predicate() -> bool:
    return False

def uncalled_function(flag: bool):
    if flag:
        pass
    elif predicate:  # error: [redundant-condition] "Function `predicate` is always truthy: Did you mean to call this function?"
        pass
    else:
        raise AssertionError
```

## Dunder methods that return `NotImplemented`

In dunder methods, it is usually more idiomatic to `return NotImplemented` rather than `raise` if
you're writing code with defensive runtime checks. We support this pattern too:

```py
class Foo:
    def __add__(self, other: "Foo") -> "Foo":
        # no diagnostic, even though this is inferred as always `True`!
        if not isinstance(other, Foo):
            return NotImplemented
        return self
```

## Tests that include walrus expressions

Walrus expressions can have side effects, so an always-true walrus expression may not always be
redundant. Examples of this can be found in CPython's scripts, where deliberately true walrus
expressions are used to continue the boolean-expression chain:

- <https://github.com/python/cpython/blob/f74cdf80a120649e4c353430da8cbd1305c00993/Tools/peg_generator/pegen/grammar_parser.py#L152-L168>
- <https://github.com/python/cpython/blob/f74cdf80a120649e4c353430da8cbd1305c00993/Tools/peg_generator/pegen/grammar_parser.py#L152-L168>

It is arguably always possible to write this kind of code in a clearer, more obvious way, so we
still emit a diagnostic on code like this, even though it may be deliberate. However, we use the
`redundant-condition-strict` rule for these patterns, so that the rule that is enabled by default is
unopinionated:

```py
def coinflip1() -> bool:
    return True

def coinflip2() -> bool:
    return True

foo = ("foo",)

# the always-truthy item is a `tuple[Literal["bar"]]`,
# so this would normally trigger `redundant-condition`,
# but the presence of the walrus expression means we use
# the disabled-by-default error code.
if coinflip1() and (foo := ("bar",)) and coinflip2():  # error: [redundant-condition-strict]
    ...
```
