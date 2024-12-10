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
