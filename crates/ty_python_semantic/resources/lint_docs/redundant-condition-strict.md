## What it does

Detects boolean conditions where the condition can be statically inferred to be always true or
always false.

This rule is disabled by default. It exclusively covers cases that its sibling (enabled-by-default)
rule `redundant-condition` does not cover. These cases often flag real bugs in user code, but also
have a significantly higher rate of unavoidable false positives than other cases.

This rule is emitted on expressions where the boolean test is inferred as evaluating to `True`
itself, `False` itself, or an exact integer such as `1` or `0`. It also is emitted on any
expression where the boolean test uses a walrus operator.

## Why is this bad?

A boolean condition that is always true or always false usually indicates a mistake in your code,
and can often lead to incorrect behavior.

## Examples

A common error in Python code is to make the mistake of thinking that indexing into a `bytes`
object will get you an object of type `bytes`. But `bytes` work differently to `str`s in Python --
although a string is a sequence of strings, a bytestring is a sequence of `int`s, so indexing into
a `bytes` object gives you an `int`. This rule can catch that error by alerting you to the fact
that comparing a `bytes` object with an `int` will always evaluate to `False`:

```py
def validate_record(data: bytes) -> None:
    if data[0] != b"\x1e":  # error: [redundant-condition-strict]
        raise ValueError("Invalid record separator")
```

Another common mistake is to assume that annotating `**kwargs` with `dict[str, str]` describes the
dictionary containing the keyword arguments. In fact, a `**kwargs` annotation describes each
individual keyword argument, so this annotation says that every value is itself a dictionary.
Comparing one of those values with a string will therefore always evaluate to `False`:

```py
def trace(**kwargs: dict[str, str]) -> None:
    if kwargs.get("operation") == "task":  # error: [redundant-condition-strict]
        print("Tracing task")
```

## Known issues

This rule can often trigger on code that is not incorrect, but could be written in a clearer way.
For example, the rule will flag this code:

```py
from enum import Enum


class YesOrNo(Enum):
    YES = 1
    NO = 0


def say_yes_or_no(what_to_say: YesOrNo):
    if what_to_say == YesOrNo.YES:
        print("yes")
    elif what_to_say == YesOrNo.NO:  # error: [redundant-condition-strict]
        print("no")
```

This snippet could be written more clearly as this, which would not trigger the rule:

```py
def say_yes_or_no(what_to_say: YesOrNo):
    if what_to_say == YesOrNo.YES:
        print("yes")
    else:
        assert what_to_say == YesOrNo.NO
        print("no")
```

or this, which would also be fine according to the rule's heuristics:

```py
from typing_extensions import assert_never


def say_yes_or_no(what_to_say: YesOrNo):
    if what_to_say == YesOrNo.YES:
        print("yes")
    elif what_to_say == YesOrNo.NO:
        print("no")
    else:
        assert_never(what_to_say)
```

In a similar vein, this rule can often flag `and` or `or` expressions that have operands which are
deliberately always truthy or deliberately always falsy, because the purpose of the operand is to
have some side effect occur. For example:

```py
import random
from typing import Literal


def want_to_go_fishing() -> bool:
    return random.choice([True, False])


def weather_report() -> Literal["rainy", "sunny", "cloudy"]:
    return random.choice(["rainy", "sunny", "cloudy"])


def have_fishing_supplies() -> bool:
    return random.choice([True, False])


def main():
    if (
        want_to_go_fishing()
        and (weather := weather_report())  # error: [redundant-condition-strict]
        and have_fishing_supplies()
    ):
        print(f"The weather is {weather}, let's go fishing")
```

The middle operand in the above `and` expression is always truthy. This might be deliberate, but
even if it is, the function would arguably be clearer if it were written like this instead:

```py
def main():
    if want_to_go_fishing():
        weather = weather_report()
        if have_fishing_supplies():
            print(f"The weather is {weather}, let's go fishing")
```

Lastly, the rule cannot reliably distinguish in all cases comparisons that are intentionally
always true/false from those that are unintentionally always true/false. The rule takes care to
avoid flagging code that uses `if TYPE_CHECKING`, `if sys.version_info < (X, Y)`,
`if sys.platform = ...` and `if os.name = ...`. But it cannot reliably determine that code like
this was written the way it was meant to:

```py
DEBUGGING = 0

if DEBUGGING:  # error: [redundant-condition-strict]
    print("Doing debugging stuff...")
```
