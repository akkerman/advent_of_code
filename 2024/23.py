"""Day 23: LAN Party."""
import fileinput
from collections import  defaultdict
from functools import cache

Edge = tuple[str, str]
Vertex = str
Network = tuple[Vertex, ...]

def part_one(vertices: set[Vertex], edges: list[Edge]) -> int:
    """Solution to part one."""
    connected: dict[Vertex, set[Vertex]] = defaultdict(set)
    for a,b in edges:
        connected[a].add(b)
        connected[b].add(a)


    three_or_more: set[tuple[Vertex, Vertex, Vertex]] = set()
    for a in connected:
        if a[0] != 't': continue
        for b in connected[a]:
            for c in connected[b]:
                if c in connected[a]:
                    three_or_more.add(tuple(sorted([a,b,c])))

    return len(three_or_more)

def part_two_brute(vertices: set[Vertex], edges: list[Edge]) -> Network:
    connected: dict[Vertex, set[Vertex]] = defaultdict(set)
    for a,b in edges:
        connected[a].add(b)
        connected[b].add(a)

    @cache
    def find_fully_connected(network: Network, current: str) -> list[Network]:
        all_networks: list[Network] = []
        for v in connected[current]:
            if v in network: continue
            for c in network:
                if v not in connected[c]:
                    all_networks.append(network)
                    break
            else:
                new_network = tuple(sorted(network + (v,)))
                networks = find_fully_connected(new_network, v)
                if networks:
                    all_networks.extend(networks)
        return all_networks

    all_networks:list[Network] = []
    for v in vertices:
        if v[0] != 't': continue
        networks: list[Network] = find_fully_connected((v,), v)
        if networks:
            all_networks.extend(networks)

    longest = max(all_networks, key=len)
    return longest




def part_two(vertices: set[Vertex], edges: list[Edge]) -> str:
    graph: dict[str, set[str]] = defaultdict(set)
    for a,b in edges:
        graph[a].add(b)
        graph[b].add(a)

    def bron_kerbosch(R: set[str], P: set[str], X: set[str]) -> set[str]:
        if not P and not X:
            return R  # clique found
        max_clique: set[str] = set()
        for v in list(P):
            candidate = bron_kerbosch(
                R.union([v]), 
                P.intersection(graph[v]), 
                X.intersection(graph[v]), 
            )
            if len(candidate) > len(max_clique):
                max_clique = candidate
            P.remove(v)
            X.add(v)
        return max_clique

    max_clique = bron_kerbosch(set(), set(vertices), set())

    return ",".join(sorted(max_clique))

def main():
    """Parse input file, pass to puzzle solvers."""
    edges: list[Edge] = []
    vertices: set[Vertex] = set()
    for line in fileinput.input():
        line = line.strip()
        a,b = line.split('-')
        vertices.add(a)
        vertices.add(b)
        edges.append((a,b))

    print('part_one', part_one(vertices, edges))

    print('part_two', part_two(vertices, edges))


if __name__ == '__main__':
    main()
