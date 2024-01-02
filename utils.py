import re

def find_occurences (pattern, line, overlap=False):
    """ find occurences of str in line """
    indices = []

    regex = re.compile(pattern)
    if overlap:
        regex = re.compile(f'(?=({pattern}))')

    for match in regex.finditer(line):
        indices.append(match.start())

    return indices
