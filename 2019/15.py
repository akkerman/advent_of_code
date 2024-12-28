"""Day 15: Oxygen System."""
import fileinput
import heapq
from collections import defaultdict
import random
from intcode import computer, IO
from abc import abstractmethod

NORTH = 1
SOUTH = 2
WEST = 3
EAST = 4

UNEXPLORED = -1
WALL = 0
MOVED = 1
FOUND = 2

Coord = tuple[int,int]

wind2dir = {
    NORTH: (0,1),
    SOUTH: (0,-1),
    WEST: (-1,0),
    EAST: (1,0)
}

class Droid(IO):
    def __init__(self):
        self.current_dir = NORTH
        self.current_pos = (0,0)
        self.maze = defaultdict[Coord, int](lambda: UNEXPLORED)
        self.oxygen_pos: Coord = (0,0)
        self.maze[self.current_pos] = MOVED

    def input(self) -> int:
        return self.move()
    def output(self, value:int) -> None:
        self.status(value)

    @abstractmethod
    def move(self) -> int:
        pass

    @abstractmethod
    def status(self, value:int) -> None:
        pass

    def next_pos(self, dir:int=0) -> Coord:
        """Determine the next position based on the current direction."""
        x,y = self.current_pos
        dx,dy = wind2dir[dir] if dir else wind2dir[self.current_dir]
        return (x+dx, y+dy)

    def print_maze(self):
        """Print ascii representation of the maze."""
        min_x = min(x for x,_ in self.maze.keys())
        max_x = max(x for x,_ in self.maze.keys())
        min_y = min(y for _,y in self.maze.keys())
        max_y = max(y for _,y in self.maze.keys())


        for y in range(max_y, min_y-1, -1):
            for x in range(min_x, max_x+1):
                status = self.maze[(x,y)]
                if (x,y) == (0,0):
                    print('D', end='')
                elif (x,y) == self.oxygen_pos or status == FOUND:
                    print('O', end='')
                elif status == UNEXPLORED:
                    print('?', end='')
                elif status == WALL:
                    # print('#', end='')
                    print('█', end='')
                elif status == MOVED:
                    print('.', end='')
                else:
                    raise ValueError('Unknown status {}'.format(status))
            print()

class DrunkenDroid(Droid):
    """Droid that moves randomly."""

    def move(self) -> int:
        """Move the droid in a random direction."""
        if len(self.maze.keys()) >= 1659:
            return 0

        self.current_dir = random.choice([1,2,3,4])
        return self.current_dir

    def status(self, value:int) -> None:
        """Record the status of the droid's move."""
        if value == WALL:
            self.maze[self.next_pos()] = WALL
            return

        self.maze[self.current_pos] = value
        self.current_pos = self.next_pos()

        if value == FOUND and self.oxygen_pos == (0,0):
            # print('Found oxygen system at', self.current_pos)
            self.oxygen_pos = self.current_pos

class DiscoveringDroid(Droid):
    DISCOVERING = 0
    BACKTRACKING = 1

    def __init__(self):
        super().__init__()
        self.unexplored = set[Coord]()
        self.path = list[Coord]()
        self.unexplored.update(self.unexplored_neighbors())
        self.state: int = DiscoveringDroid.DISCOVERING


    def unexplored_neighbors(self) -> list[Coord]:
        """Return the unexplored neighbors of the current position."""
        candidates = [self.next_pos(dir) for dir in [NORTH, SOUTH, WEST, EAST]]
        return [nb for  nb in candidates if self.maze[nb] == UNEXPLORED]

    def move(self) -> int:
        """Discover or backtrack the maze."""
        if not self.unexplored:
            return 0

        unexplored = self.unexplored_neighbors()
        if unexplored:
            self.state = DiscoveringDroid.DISCOVERING
            self.unexplored.update(unexplored)
            # print('want to move to', unexplored[0])
            self.current_dir = self.direction(self.current_pos, unexplored[0])
            return self.current_dir

        return self.backtrack()


    def backtrack(self) -> int:
        """Backtrack the maze."""
        if self.unexplored and not self.path:
            raise ValueError('Unexplored but no path')

        if not self.path:
            return 0

        self.state = DiscoveringDroid.BACKTRACKING
        self.current_dir = self.direction(self.current_pos, self.path.pop())
        return self.current_dir

    def status(self, value:int) -> None:
        next_pos = self.next_pos()
        self.unexplored.discard(next_pos)

        if self.state == DiscoveringDroid.BACKTRACKING:
            self.current_pos = next_pos
            return

        self.maze[next_pos] = value
        if value == WALL: return

        self.path.append(self.current_pos)
        self.current_pos = next_pos

        if value == FOUND:
            self.oxygen_pos = self.current_pos


    def direction(self, start: Coord, end: Coord) -> int: 
        if start == end:
            raise ValueError('Same position')
        if (abs(start[0] - end[0]) + abs(start[1] - end[1])) > 1:
            raise ValueError(f'{end} is more than one step away from {start}')

        if start[0] == end[0]:
            return NORTH if start[1] < end[1] else SOUTH

        if start[1] == end[1]:
            return EAST if start[0] < end[0] else WEST

        raise ValueError('Invalid direction')


def part_one(maze: dict[Coord, int], end: Coord) -> int:
    """Solution to part one."""
    q = [(0,0,0)]
    visited:set[Coord] = set()
    while q:
        steps, x, y = heapq.heappop(q)
        if (x,y) == end:
            return steps
        if (x,y) in visited: continue
        visited.add((x,y))
        for dx,dy in wind2dir.values():
            nx,ny = x+dx, y+dy
            if maze[(nx,ny)] == WALL:
                continue
            heapq.heappush(q, (steps+1, nx, ny))

    raise ValueError('No path found')


def part_two(maze: dict[Coord, int], end: Coord) -> int:
    """Solution to part two."""
    q = [(0, *end)]
    visited = set[Coord]()
    mins_to_fill = 0
    while q:
        mins, x, y = heapq.heappop(q)
        if (x,y) not in maze:
            continue
        if maze[(x,y)] == WALL:
            continue
        if (x,y) in visited: continue
        mins_to_fill = max(mins_to_fill, mins)
        visited.add((x,y))
        for dx,dy in wind2dir.values():
            nx,ny = x+dx, y+dy
            heapq.heappush(q, (mins+1, nx, ny))

    
    return mins_to_fill


def main():
    """Parse input file, pass to puzzle solvers."""
    program = list[int]()
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))


    # droid = DrunkenDroid()
    droid = DiscoveringDroid()
    computer(program, droid)
    print('part_one', part_one(droid.maze, droid.oxygen_pos))
    print('part_two', part_two(droid.maze, droid.oxygen_pos))


if __name__ == '__main__':
    main()
