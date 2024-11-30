# Correct

for _ in range(5):
    pass

_valid_type = int

_valid_var_1: _valid_type

_valid_var_1 = 1

_valid_var_2 = 2

_valid_var_3 = _valid_var_1 + _valid_var_2

def _valid_fun():
    pass

_valid_fun()

def fun(arg):
    _valid_unused_var = arg
    pass

_x = "global"

def fun():
    global _x
    return _x

def fun():
    __dunder__ = "dunder variable"
    return __dunder__

def fun():
    global _x
    _x = "reassigned global"
    return _x

class _ValidClass:
    pass

_ValidClass()

class ClassOk:
    _valid_private_cls_attr = 1

    print(_valid_private_cls_attr)

    def __init__(self):
        self._valid_private_ins_attr = 2
        print(self._valid_private_ins_attr)

    def _valid_method(self):
        return self._valid_private_ins_attr

    def method(arg):
        _valid_unused_var = arg
        return

def fun(x):
    _ = 1
    __ = 2
    ___ = 3
    if x == 1:
        return _
    if x == 2:
        return __
    if x == 3:
        return ___
    return x

# Incorrect

class Class_:
    def fun(self):
        _var = "method variable" # [RUF052]
        return _var

def fun(_var): # [RUF052]
    return _var

def fun():
    _list = "built-in" # [RUF052]
    return _list

x = "global"

def fun():
    global x
    _x = "shadows global" # [RUF052] (but unfixable)
    return _x

def foo():
  x = "outer"
  def bar():
    nonlocal x
    _x = "shadows nonlocal" # [RUF052] (but unfixable)
    return _x
  bar()
  return x

def fun():
    x = "local"
    _x = "shadows local" # [RUF052] (but unfixable)
    return _x

def fun2():
    x = "local"
    x_ = "also local"
    _x = "shadows local"  # [RUF052] (but unfixable)
    return _x

def fun3():
    global x_
    x = "local"
    _x = "shadows local"  # [RUF052] (but unfixable)
    return _x

def fun4():
    x_ = 42
    def fun5():
        x = "local"
        _x = "shadows local"  # RUF052 (but unfixable)
        print(x_)
        return _x
