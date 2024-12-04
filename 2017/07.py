# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re
from typing import List, Tuple
from collections import Counter

Vertex = Tuple[str, int, int]
Edge = Tuple[str, str]

def part_one(_:List[Vertex], edges:List[Edge]) -> set[str]:
    """ part one """
    sources = {edge[0] for edge in edges}
    targets = {edge[1] for edge in edges}
    return sources - targets

def neigbors(edges:List[Edge], name:str) -> List[str]:
    return [edge[1] for edge in edges if edge[0] == name]

def calc_total_weight(vertices:List[Vertex], edges:List[Edge], name:str) -> List[Vertex]:
    """ calc_total_weight """
    
    dictionary = {vertex[0]: vertex for vertex in vertices}

    updated: List[Vertex] = []

    def traverse(name:str) -> int:
        vertex = dictionary[name]
        nbrs = neigbors(edges, name)
        if nbrs is []: # leaf
            updated.append(( vertex[0], vertex[1], vertex[1] ))
            return dictionary[name][1]

        total = vertex[1] + sum([traverse(nbr) for nbr in nbrs])
        updated.append(( vertex[0], vertex[1], total ))
        return total


    traverse(name)
    return updated

def rebalance(vertices:List[Vertex], edges:List[Edge]) -> int:
    dictionary = {vertex[0]: vertex for vertex in vertices}
    for vertex in vertices:
        nbrs = neigbors(edges, vertex[0])
        if nbrs is []:
            continue

        weights = Counter([dictionary[nbr][2] for nbr in nbrs])
        if len(weights) != 2:
            continue

        target = 0
        actual = 0
        for weight, count in weights.items():
            if count == 1:
                actual = weight
            else:
                target = weight


        vertex_to_adjust = [dictionary[nbr] for nbr in nbrs if dictionary[nbr][2] == actual][0]

        return vertex_to_adjust[1] + (target - actual)


    return -1


def part_two(vertices:List[Vertex], edges:List[Edge]):
    """ part two """
    root = part_one(vertices, edges).pop()
    updated = calc_total_weight(vertices, edges, root)
    return rebalance(updated, edges)


def main():
    """ main """
    vertices: List[Vertex] = []
    edges: List[Edge] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        name, weight = re.findall(r'([a-z]+) \((\d+)\)', line)[0]
        vertices.append((name, int(weight), 0))

        if '->' in line:
            nodes = line.split(' -> ')[1].split(', ')
            for node in nodes:
                edges.append((name, node))


    print('part_one', part_one(vertices, edges))

    print('part_two', part_two(vertices, edges))


main()
