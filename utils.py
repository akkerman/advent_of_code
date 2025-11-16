from typing import Callable, TypeVar, Any
from collections.abc import Sequence
import re

def find_occurences(pattern:str, line:str, overlap:bool=False):
    """Find occurences of str in line."""
    indices:list[int] = []

    regex = re.compile(pattern)
    if overlap:
        regex = re.compile(f'(?=({pattern}))')

    for match in regex.finditer(line):
        indices.append(match.start())

    return indices

I = TypeVar('I')
def chunk_list(lst: list[I], size: int) -> list[list[I]]:
    """Chunk a list into smaller lists of given size."""
    return [lst[i:i + size] for i in range(0, len(lst), size)]

T = TypeVar('T', bound=Callable[..., Any])
def perf_timer(func: T) -> T:
    """Decorator to time function performance."""
    import time
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        duration = end - start
        if duration < 0.001:
            print(f'{func.__name__} took {duration * 1000000:.2f} microseconds')
        elif duration < 1:
            print(f'{func.__name__} took {duration * 1000:.2f} milliseconds')
        else:
            print(f'{func.__name__} took {duration:.2f} seconds')
        return result
    return wrapper

def manhattan(a: Sequence[int], b: Sequence[int]) -> int:
    """Calculate the Manhattan distance between two point/vectors."""
    return sum(abs(x - y) for x, y in zip(a, b))
