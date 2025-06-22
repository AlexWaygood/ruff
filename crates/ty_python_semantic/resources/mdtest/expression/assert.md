## Condition with object that implements `__bool__` incorrectly

```py
class NotBoolable:
    __bool__: int = 3

# error: [unsupported-bool-conversion] "Boolean conversion is unsupported for object of type `NotBoolable`"
assert NotBoolable()
```
