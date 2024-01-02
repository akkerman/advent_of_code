import re

def find_occurences (pattern, line):
    """ find occurences of str in line """
    indices = []
    for m in re.finditer(pattern, line):
        indices.append(m.start())

    return indices
