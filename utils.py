from typing import Callable, TypeVar, Any
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

T = TypeVar('T', bound=Callable[..., Any])
def perf_timer(func: T) -> T:
    """Decorator to time function performance."""
    import time
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        duration = end - start
        if duration < 1:
            print(f'{func.__name__} took {duration * 1000:.2f} milliseconds')
        else:
            print(f'{func.__name__} took {duration} seconds')
        return result
    return wrapper
