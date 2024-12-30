"""Day 12: The N-Body Problem."""
import fileinput
import re
import math


Position = list[int]
Velocity = list[int]

def apply_gravity(positions: list[Position], velocities: list[Velocity]) -> list[Velocity]:
    """Calculate new velocities."""
    new_velocities = list[Velocity]()
    for i, moon in enumerate(positions):
        v: Velocity = velocities[i].copy()
        for other in positions:
            if moon == other:
                continue
            dv: Velocity = [signum(m, o) for m, o in zip(moon, other)]
            v = add(v, dv)
        new_velocities.append(v)
    return new_velocities

def apply_velocity(positions: list[Position], velocities: list[Velocity]) -> list[Position]:
    """Update positions based on velocities."""
    return [add(p, v) for p, v in zip(positions, velocities)]

def calculate_energy(positions: list[list[int]], velocities: list[list[int]]) -> int:
    """Calculate the total energy of the system."""
    # Total energy is the sum of the potential and kinetic energy of each moon.
    return sum(abs_sum(p) * abs_sum(v) for p, v in zip(positions, velocities))

def signum(x: int, y: int) -> int:
    """Return the sign of y - x."""
    return (y > x) - (y < x)

def add (xs: list[int], ys: list[int]) -> Position:
    """Add two lists element-wise"""
    # i.e: update velocity or apply velocity to position
    return [x + y for x, y in zip(xs, ys)]

def abs_sum(xs: list[int]) -> int:
    """Return the sum of the absolute values in the list."""
    return sum(abs(x) for x in xs)

def part_one(positions: list[Position]) -> int:
    """Calculate the total energy in the system after 1000 steps."""
    velocities = [[0,0,0] for _ in range(len(positions))]
    for _ in range(1000):
        velocities = apply_gravity(positions, velocities)
        positions = apply_velocity(positions, velocities)

    return calculate_energy(positions, velocities)

def apply_component(positions: list[int], velocities: list[int]):
    """Apply 1 step of gravity and velocity to one of the components of all planets."""
    length = len(positions)
    new_velocities = velocities.copy()
    for x in range(length):
        new_velocities[x] += sum(signum(positions[x], positions[y]) for y in range(length) if x!=y)
    velocities = new_velocities
    return [p+v for p,v in zip(positions, velocities)], new_velocities    

def part_one_alt(positions: list[Position]) -> int:
    """Calculate the total energy in the system after 1000 steps."""
    xs, ys, zs = map(list, zip(*positions))
    vxs, vys, vzs = [[0]*len(xs) for _ in range(3)]

    for _ in range(1000):
        xs, vxs = apply_component(xs, vxs)
        ys, vys = apply_component(ys, vys)
        zs, vzs = apply_component(zs, vzs)

    positions =  [list(pos) for pos in zip(xs, ys, zs)]
    velocities = [list(vel) for vel in zip(vxs, vys, vzs)]

    return calculate_energy(positions, velocities)


def component_repeats(positions: list[int]) -> int:
    """Figure out when the same x,y or z position, including velocity repeats."""
    velocities = [0] * len(positions)
    history = {tuple(positions + velocities)}

    while True:
        positions, velocities = apply_component(positions, velocities)
        vector = tuple(positions + velocities)
        if vector in history:
            return len(history)
        history.add(vector)

def part_two(positions: list[Position]) -> int:
    """Find the number of steps it takes before the universe repeats."""
    xs = [p[0] for p in positions]
    ys = [p[1] for p in positions]
    zs = [p[2] for p in positions]

    repeat_xs = component_repeats(xs)
    repeat_ys = component_repeats(ys)
    repeat_zs = component_repeats(zs)
    return math.lcm(repeat_xs, repeat_ys, repeat_zs)

def part_two_alt(positions: list[Position]) -> int:
    """Find the number of steps it takes before the universe repeats."""
    return math.lcm(*map(component_repeats, map(list, zip(*positions))))

def main():
    """Parse input file, pass to puzzle solvers."""
    positions = list[Position]()
    for line in fileinput.input():
        line = line.strip()
        line = re.findall(r'-?\d+', line)
        
        positions.append(list(map(int, line)))

    print('part_one', part_one(positions))
    print('part_one', part_one_alt(positions))

    print(' '*40)
    print('part_two', part_two(positions))
    print('part_two', part_two_alt(positions))


if __name__ == '__main__':
    main()
