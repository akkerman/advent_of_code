"""Day 7: The Sum of Its Parts."""
import fileinput
import re
from collections import  defaultdict

def part_one(instructions: list[tuple[str,str]]) -> str:
    """Solution to part one."""
    deps = parse_dependencies(instructions)

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



def part_two(instructions: list[tuple[str,str]], workers:int=5) -> int:
    """Solution to part two."""
    deps = parse_dependencies(instructions)
    return simulate(deps, workers)

def simulate(deps: defaultdict[str, set[str]], workers: int) -> int:
    """Simulate the work process and return total time taken."""
    finished: set[str] = set()
    in_progress: list[tuple[int, str]] = []
    time = 0

    while len(finished) < len(deps):
        if len(in_progress) < workers:
            free_workers = workers - len(in_progress)

            running = {s for _,s in in_progress}
            available = available_steps(deps, finished, running)

            for step in available[:free_workers]:
               in_progress.append((finish_time(time, step), step))

        time, done, in_progress = advance_time(in_progress)
        finished.update(done)

    return time


def parse_dependencies(instructions: list[tuple[str,str]]) -> defaultdict[str, set[str]]:
    """Parse instructions into a dependency graph."""
    deps:defaultdict[str, set[str]]= defaultdict(set)
    for x,y in instructions:
        deps[y].add(x)
        _ = deps[x] # ensure every node is present
    return deps


def available_steps(deps: defaultdict[str, set[str]], finished: set[str], in_progress: set[str]) -> list[str]:
    """Return a sorted list of available steps."""
    return sorted(
        step for step, prereqs in deps.items()
        if step not in finished 
        and step not in in_progress 
        and not (prereqs - finished)
    )


def advance_time(in_progress: list[tuple[int, str]]) -> tuple[int, list[str], list[tuple[int, str]]]:
    """Advance time to the next completion, return time advanced, done steps, remaining in-progress."""
    time = min(t for t, _ in in_progress)
    done = [s for t, s in in_progress if t == time]
    remaining = [(t, s) for t, s in in_progress if t > time]
    return time, done, remaining

def finish_time(time: int, step: str) -> int:
    """Calculate finishing time for a step starting at given time."""
    return time + cost(step)

ORD_A = ord('A')
def cost(step: str)->int:
    """Calculate cost of a step."""
    return 61 + ord(step) - ORD_A



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
