# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List, Tuple


def part_one(orbit_map: List[Tuple[str,str]]):
    """ part one """

    def count_orbits(start: str, depth: int) -> int:
        count: int = depth
        for planet in [o for p,o in orbit_map if p == start]:
            count += count_orbits(planet, depth + 1)
        return count

    return count_orbits('COM', 0)

def part_two(orbit_map: List[Tuple[str,str]]):
    """ part two """
    def find_path(start: str, end: str):
        if start == end:
            return [end]
        paths: List[List[str]] = []
        planets = [o for p,o in orbit_map if p == start]
        if not planets:
            return None
        for planet in planets:
            path = find_path(planet, end)
            if path:
                paths.append([start] + path)
        
        if not paths:
            return None
        return min(paths, key=len)

    path1 = find_path('COM', 'YOU')
    path2 = find_path('COM', 'SAN')

    assert path1 and path2

    return len(set(path1) ^ set(path2)) - 2

def main():
    """ main """
    orbit_map: List[Tuple[str,str]] = []
    for line in sys.stdin:
        line = tuple(line.strip().split(')'))
        assert len(line) == 2
        orbit_map.append(line)

    print('part_one', part_one(orbit_map))

    print('part_two', part_two(orbit_map))


main()
