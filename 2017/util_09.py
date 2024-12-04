# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
from typing import List

def remove_garbage(line:str):
    output = ''
    in_garbage = False
    ignore_next = False
    for char in line:
        if ignore_next:
            ignore_next = False
            continue
        if char == '<' and not in_garbage:
            in_garbage = True
            continue
        if char == '>' and in_garbage:
            in_garbage = False
            continue
        if char == '!':
            ignore_next = True
            continue
        if in_garbage:
            continue
        output += char
    return output

def score_groups(line:str):
    score = 0
    nesting = 0
    for char in line:
        if char == '{':
            nesting += 1
        if char == '}':
            score += nesting
            nesting -= 1
    return score

def take_garbage(line:str) -> str:
    output = ''
    in_garbage = False
    ignore_next = False
    for char in line:
        if ignore_next:
            ignore_next = False
            continue
        if char == '<' and not in_garbage:
            in_garbage = True
            continue
        if char == '>' and in_garbage:
            in_garbage = False
            continue
        if char == '!':
            ignore_next = True
            continue
        if in_garbage:
            output += char
            continue
    return output

