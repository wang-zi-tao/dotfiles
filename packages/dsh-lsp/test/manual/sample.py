# Fixture for diagnostics-e2e.mjs: five deliberate type errors.
from typing import List


def add(a: int, b: int) -> int:
    return a + b


def wrong_return() -> int:
    return "not an int"


def wrong_assign() -> None:
    x: int = "text"


def calls_with_wrong_arity() -> int:
    return add(1)


def uses_unknown_name() -> int:
    return undefined_function(2)


def bad_list_element() -> List[int]:
    return [1, 2, "three"]
