# Wildcard ("star") imports

## Basic wildcard imports

If a `bar` module has a wildcard import (e.g. `from foo import *`)
in its global namespace, all symbols from the global namespace of
`foo` are available in the global namespace of `bar`. Star imports are a
syntax error if they appear in any scope except the global scope.

`foo.py`:

```py
X = 42
```

`bar.py`:

```py
from foo import *

reveal_type(X)  # revealed: Unknown | Literal[42]
```
