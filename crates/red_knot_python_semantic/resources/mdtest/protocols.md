# `typing.Protocol`

`typing.Protocol` allows a user to declare a type that uses structural subtyping rather than
nominal subtyping.

## Defining a `Protocol`

If `Protocol` is present in the class's bases, all other bases must either be `object`,
`Generic[]`, or a `Protocol` subclass. Otherwise, the class is invalid:

```py
from typing import Protocol, Generic, TypeVar

T = TypeVar("T")

class Invalid(object, Protocol): ...
class Invalid1(int, Protocol): ...

class Valid(Protocol, object): ...
class Valid1(Protocol, Generic[T]): ...
class Valid2(Generic[T], Protocol): ...
class Valid3(Valid, Protocol): ...

class Invalid2(Valid, int, Protocol): ...
```
