## What it does

Detects boolean conditions where the condition can be statically inferred to be always true or
always false.

This rule is enabled by default, and is deliberately not comprehensive. In order to avoid false
positives, it is not emitted on any expression where the boolean test is inferred as evaluating to
`True` itself, `False` itself, or an exact integer such as `1` or `0`. It also is not emitted on
any expression where the boolean test uses a walrus operator. These cases are all covered by
`redundant-condition-strict`, a sibling rule to this one that is disabled by default and which
exclusively covers cases that are exempted by this rule.

## Why is this bad?

A boolean condition that is always true or always false usually indicates a mistake in your code,
and can often lead to incorrect behavior.

## Examples

A common error that triggers this rule is to forget to call a function, for example:

```py
import random


def should_do_action() -> bool:
    return random.choice([True, False])


# oops! You forgot the parentheses here... this should have been `if should_do_action()`.
# Because it's not, this will always be `True`:
if should_do_action:  # error: [redundant-condition]
    print("Doing stuff...")
```

Another common mistake is to forget to `await` a coroutine:

```py
import random


async def should_do_async_action():
    return random.choice([True, False])


async def main():
    # oops! Forgot the await here... this should have been `if await should_do_async_action()`.
    # Because it's not, this will always be `True`:
    if should_do_async_action():  # error: [redundant-condition]
        print("Doing stuff async...")
```

Or to forget that `tuple[X]` means "A tuple with exactly one element" rather than "a tuple with an
arbitrary number of elements" (for which you'd use `tuple[X, ...]`):

```py
# you almost certainly meant to write `tuple[str, ...]` here rather than `tuple[str]`...
def consume_tuples(x: tuple[str]):
    # ...and that means that this later condition is inferred as always being True by ty:
    if x:  # error: [redundant-condition]
        print("Got a non-empty tuple")
```

Some Pythonistas fall into the trap of thinking that a generator expression will be falsy if it has
zero elements inside it -- but generator expressions are lazy, and so they're always truthy unless
you collect them into a tuple:

```py
def test_my_data(data: list[int]):
    # this will always be `True`, because the asserted object is a `types.GeneratorType` instance,
    # not a `tuple`! `assert any(item for item in data if item > 42)`
    # is probably what you meant instead.
    assert (item for item in data if item > 42)  # error: [redundant-condition]
```

## Known issues

This rule can sometimes flag boolean expressions involving always-true objects that may not be
buggy. For example, the following code is valid Python, but is flagged by the rule:

```py
def f(): ...
def g(): ...


def test(coinflip: bool):
    func = coinflip and f or g  # error: [redundant-condition]
    func()
```

However, this code is far from idiomatic. To avoid the diagnostic being emitted, use an `if`/`else`
conditional expression instead, which has the added benefit of being easier to read:

```py
def test(coinflip: bool):
    func = f if coinflip else g
    func()
```

Another known issue is that if an always-truthy condition is an awaitable, this rule sometimes
offers an unsafe fix to add `await`. Applying this fix inside a synchronous generator expression
turns it into an asynchronous generator, but the consumer of the generator may also need to change
if this fix is applied. For example, `list(...)` and a regular `for` loop cannot consume an
asynchronous generator. Review the surrounding code before applying this fix.
