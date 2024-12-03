# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re
from typing import List, Tuple

Vertex = Tuple[str, int]
Edge = Tuple[str, str]

def part_one(_:List[Vertex], edges:List[Edge]) -> set[str]:
    """ part one """
    sources = {edge[0] for edge in edges}
    targets = {edge[1] for edge in edges}
    return sources - targets


def part_two(vertices:List[Vertex], edges:List[Edge]):
    """ part two """
    return 'todo'


def main():
    """ main """
    vertices: List[Tuple[str, int]] = []
    edges: List[Tuple[str, str]] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        name, weight = re.findall(r'([a-z]+) \((\d+)\)', line)[0]
        vertices.append((name, int(weight)))

        if '->' in line:
            nodes = line.split(' -> ')[1].split(', ')
            for node in nodes:
                edges.append((name, node))


    print('part_one', part_one(vertices, edges))

    print('part_two', part_two(vertices, edges))


main()
