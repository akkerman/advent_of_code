"""2025 Day 10: Factory"""
from collections import deque
import fileinput
from z3 import Int, Optimize, Sum

Lights = list[bool]
Button = list[int]
Joltage = list[int]
Machine = tuple[Lights, list[Button], Joltage]

VariableName = str
Target = int
Equation = tuple[list[VariableName], Target]

def min_presses_light(machine: Machine) -> int:
    """Simulate machine switching and return number of steps to get the desired combination of lights on."""
    end, buttons, _ = machine

    queue: deque[tuple[int, Lights]] = deque()
    queue.append((0, [False] * len(machine[0])))
    visited: set[tuple[bool, ...]] = set()

    while queue:
        presses, lights = queue.popleft()
        if lights == end:
            return presses

        state = tuple(lights)
        if state in visited:
            continue
        visited.add(state)
        
        for button in buttons:
            new_lights = lights[:]
            for idx in button:
                new_lights[idx] = not new_lights[idx]
            queue.append((presses + 1, new_lights))

    assert False, "Shouldn't reach here"

def part_one(machines: list[Machine]):
    """Solution to part one."""
    return sum(min_presses_light(m) for m in machines)


def min_presses_joltage_solver(equations: list[Equation]) -> int:
    """Solve the joltage equations using Z3 solver."""
    variables = {name: Int(name) 
                 for variable_names, _ in equations 
                 for name in variable_names}

    o = Optimize()

    sorted_variables = [variables[name] for name in sorted(variables)]
    for v in sorted_variables:
        o.add(v >= 0)

    for eq, target in equations:
        o.add(Sum(*[variables[v] for v in eq]) == target)

    h = o.minimize(Sum(*sorted_variables))

    o.check()
    # print(o.model())
    return h.value().as_long()

def determine_equations(machine: Machine) -> list[Equation]:
    """Determine the system of joltage equations for the machine."""
    _, buttons, joltage = machine

    equations = list[tuple[list[str], int]]()
    for idx, jolt in enumerate(joltage):
        equation = list[str]()
        for b_idx, button in enumerate(buttons):
            if idx in button:
                equation.append(f"x{b_idx}")
        equations.append((equation, jolt))
    return equations

def part_two(machines: list[Machine]):
    """Solution to part two."""
    return sum(min_presses_joltage_solver(determine_equations(m)) for m in machines)

    


def parse(button: str):
    """Parse button string into tuple."""
    res = button.strip('()')
    return [int(x) for x in res.split(',')]

def main():
    """Parse input file, pass to puzzle solvers."""
    machines = list[Machine]()
    for line in fileinput.input():
        parts  = line.strip().split(' ')
        lights = [s == '#' for s in parts[0].strip(r'\[\]')]
        buttons = [parse(button) for button in parts[1:-1]]
        joltage = [int(x) for x in parts[-1].strip('{}').split(',')]

        machines.append((lights, buttons, joltage))


    print('part_one', part_one(machines))

    print('part_two', part_two(machines))


if __name__ == '__main__':
    main()
