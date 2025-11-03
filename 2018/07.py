"""Day 7: The Sum of Its Parts."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
from typing import Callable


def solve(start: str, deps: dict[str, set[str]])-> str|None:
    """Solve the dependency graph from start."""
    def S(finished: str) -> str|None:
        if len(finished) == len(deps):
            return ''.join(finished)

        candidates: list[str] = []

        for candidate, predecessors in deps.items():
            if candidate in finished:
                continue
            if predecessors - set(finished):
                continue
            candidates.append(candidate)

        for next in sorted(candidates):
            solution = S(finished + next)
            if solution:
                return solution

        return None

    return S(start)




def part_one_old(instructions: list[tuple[str,str]] ):
    """Solution to part one."""
    deps:defaultdict[str, set[str]]= defaultdict(set)
    for x,y in instructions:
        deps[y].add(x)
        _ = deps[x] # ensure every node is present

    possible_starts = [k for k,v in deps.items() if not v]

    for start in sorted(list(possible_starts)):
        solution = solve(start, deps)
        if solution:
            return solution

    return None


# deterministic version
def part_one(instructions: list[tuple[str,str]]) -> str:
    deps:defaultdict[str, set[str]]= defaultdict(set)
    for x,y in instructions:
        deps[y].add(x)
        _ = deps[x] # ensure every node is present

    finished: list[str] = []
    while len(finished) < len(deps):
        available = [
            step
            for step, prereqs in deps.items()
            if step not in finished and prereqs <= set(finished)
        ]
        next_step = min(available)
        finished.append(next_step)

    return ''.join(finished)

def cost_example(step: str)->int:
    return 1 + ord(step) - ord('A')
def cost_aoc(step: str)->int:
    return 61 + ord(step) - ord('A')

def part_two(instructions: list[tuple[str,str]], workers:int=5, cost: Callable[[str],int] = cost_aoc ) -> int:
    """Solution to part two."""
    deps:defaultdict[str, set[str]]= defaultdict(set)
    for x,y in instructions:
        deps[y].add(x)
        _ = deps[x] # ensure every node is present

    finished: list[str] = []
    in_progress: list[tuple[int, str]] = []
    time = 0

    while len(finished) < len(deps):

        if len(in_progress) < workers:
            available:list[str] = []
            running = [s for _,s in in_progress]
            for step, prereqs in deps.items():
                if step in finished:
                    continue
                if step in running:
                    continue
                if prereqs - set(finished):
                    continue
                available.append(step)

            to_add = min(workers-len(in_progress), len(available))

            for next_step in sorted(available)[:to_add]:
               in_progress.append((time+cost(next_step), next_step))

        time += 1

        for t, s in in_progress:
            if t == time:
                finished.append(s)

        in_progress = [(t,s) for t,s in in_progress if t > time]

    return time

STEP_RE = re.compile(r'Step (\w) must be finished before step (\w) can begin.')

def main():
    """Parse input file, pass to puzzle solvers."""
    instructions: list[tuple[str,str]] = []
    for line in fileinput.input():
        line = line.strip()
        assert (res := STEP_RE.match(line)), f'Bad line: {line}'
        instructions.append((res[1], res[2]))

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))


if __name__ == '__main__':
    main()


instructions_example = [('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]
workers_example = 2

def test_part_two():
    assert part_two(instructions_example, workers_example, cost_example) == 15
