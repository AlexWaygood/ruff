# Imports

## Star-imports

```py
from foo import *

reveal_type(X)  # revealed: Literal[1]
print(_Y)
```

```py path=foo.py
X = 1
_Y = 2
```
