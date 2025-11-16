"""2017 Day 20: Particle Swarm"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Vector = tuple[int, int, int]
ParticleSwarm = dict[int,tuple[Vector, Vector, Vector]]

re_vector = r'<(-?\d+),(-?\d+),(-?\d+)>'
re_particle = re.compile(fr'p={re_vector}, v={re_vector}, a={re_vector}')

def add(v1: Vector, v2: Vector) -> Vector:
    """Add two 3D vectors."""
    return (v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2])

def parse_line(line:str) -> tuple[Vector, Vector, Vector]:
    """Parse input line."""
    if match := re_particle.match(line):
        nums: list[int] = list(map(int, match.groups()))
        assert len(nums) == 9
        position = (nums[0], nums[1], nums[2])
        velocity = (nums[3], nums[4], nums[5])
        acceleration = (nums[6], nums[7], nums[8])
        return (position, velocity, acceleration)
    raise ValueError(f'Cannot parse line: {line}')

def simulate_step(particles: ParticleSwarm) -> ParticleSwarm:
    """Simulate one time step for all particles."""
    new_particles: ParticleSwarm = {}
    for id, (position, velocity, acceleration) in particles.items():
        new_velocity = add(velocity, acceleration)
        new_position = add(position, new_velocity)
        new_particles[id] = (new_position, new_velocity, acceleration)
    return new_particles

def minimum(particles: ParticleSwarm) -> int:
    """Find the particle closest to origin."""
    min_distance: int|None = None
    min_id: int|None = None
    for id, vectors in particles.items():
        position = vectors[0]
        distance = sum(abs(x) for x in position)
        if min_distance is None or distance < min_distance:
            min_distance = distance
            min_id = id
    assert min_id is not None
    return min_id

def part_one(particles: ParticleSwarm):
    """Solution to part one."""
    ids: deque[int] = deque()
    while True:
        ids.append(minimum(particles))
        if len(ids) > 1000:
            ids.popleft()
            if all(x == ids[0] for x in ids):
                return ids[0]
        particles = simulate_step(particles)


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    particles: ParticleSwarm = {}
    for id, line in enumerate(fileinput.input()):
        line = line.strip()
        p,v,a = parse_line(line)
        particles[id] = (p,v,a)


    # too low 255
    print('part_one', part_one(particles))

    print('part_two', part_two(particles))


if __name__ == '__main__':
    main()
