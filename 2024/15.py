"""Day 15: Warehouse Woes."""
import sys

Coord = tuple[int, int]

def move(coord:Coord, direction:Coord) -> Coord:
    return (coord[0] + direction[0], coord[1] + direction[1])


def sum_gps(boxes:set[Coord]) -> int:
    return sum(100 * box[0] + box[1] for box in boxes)

def part_one(walls:set[Coord], boxes:set[Coord], directions:list[Coord], start:Coord):
    """Move robot and boxes."""

    def move_boxes(coord:Coord, direction:Coord) -> bool:
        next = move(coord, direction)
        if next in walls:
            return False
        if next not in boxes:
            boxes.remove(coord)
            boxes.add(next)
            return True
        if move_boxes(next, direction):
            boxes.remove(coord)
            boxes.add(next)
            return True
        return False

    robot = start
    for dir in directions:
        next = move(robot, dir)
        if next in walls:
            continue
        if next in boxes:
            if move_boxes(next, dir):
                robot = next
            continue
        robot = next

    return sum_gps(boxes)


def print_warehouse(walls:set[Coord], left_boxes:set[Coord], right_boxes:set[Coord], robot:Coord, dir:Coord):

    direction = {
        (0,0): 'Initial state:',
        (-1, 0):'Move ^:',
        (1, 0) :'Move v:',
        (0, -1):'Move <:',
        (0, 1) :'Move >:'
    }
    print(direction[dir])


    for row in range(7):
        for col in range(14):
            if (row, col) in walls:
                print('#', end='')
            elif (row, col) in left_boxes:
                print('[', end='')
            elif (row, col) in right_boxes:
                print(']', end='')
            elif (row, col) == robot:
                print('@', end='')
            else:
                print('.', end='')
        print()
    print()



def part_two(walls:set[Coord], left_boxes:set[Coord], right_boxes:set[Coord], directions:list[Coord], start:Coord) -> int:
    """Solution to part two."""

    print_warehouse(walls, left_boxes, right_boxes, start, (0, 0))

    def move_boxes(coord: Coord, direction:Coord):
        orig_left_boxes = left_boxes.copy()
        orig_right_boxes = right_boxes.copy()

        def __move_boxes(coord:Coord, direction:Coord) -> bool:
            r,c = coord
            left = coord if coord in left_boxes else (r, c-1)
            right = coord if coord in right_boxes else (r, c+1)

            next_left = move(left, direction)
            next_right = move(right, direction)

            if next_left in walls or next_right in walls:
                return False

            if direction[1] == -1: # left
                if next_left not in right_boxes or __move_boxes(next_left, direction):
                    left_boxes.remove(left)
                    right_boxes.remove(right)
                    left_boxes.add(next_left)
                    right_boxes.add(next_right)
                    return True
                return False

            if direction[1] == 1: # right
                if next_right not in left_boxes or __move_boxes(next_right, direction):
                    left_boxes.remove(left)
                    right_boxes.remove(right)
                    left_boxes.add(next_left)
                    right_boxes.add(next_right)
                    return True
                return False


            ## up or down
            boxes = left_boxes | right_boxes
            if next_left not in boxes and next_right not in boxes:
                left_boxes.remove(left)
                right_boxes.remove(right)
                left_boxes.add(next_left)
                right_boxes.add(next_right)
                print('neither '*10)
                print_warehouse(walls, left_boxes, right_boxes, (0,0), direction)
                return True
            if next_right in boxes and __move_boxes(next_right, direction):
                left_boxes.remove(left)
                right_boxes.remove(right)
                left_boxes.add(next_left)
                right_boxes.add(next_right)
                print('right '*10)
                print_warehouse(walls, left_boxes, right_boxes, (0,0), direction)
                return True
            if next_left in boxes and __move_boxes(next_left, direction):
                print('left')
                left_boxes.remove(left)
                right_boxes.remove(right)
                left_boxes.add(next_left)
                right_boxes.add(next_right)
                print('left '*10)
                print_warehouse(walls, left_boxes, right_boxes, (0,0), direction)
                return True
            if move_boxes(next_left, direction) and move_boxes(next_right, direction):
                left_boxes.remove(left)
                right_boxes.remove(right)
                left_boxes.add(next_left)
                right_boxes.add(next_right)
                print('both '*10)
                print_warehouse(walls, left_boxes, right_boxes, (0,0), direction)
                return True

            return False

        if not __move_boxes(coord, direction):
            # rollback
            left_boxes.clear()
            right_boxes.clear()
            left_boxes.update(orig_left_boxes)
            right_boxes.update(orig_right_boxes)
            return False

        return True

    robot = start
    for dir in directions:
        next = move(robot, dir)
        if next in walls:
            print_warehouse(walls, left_boxes, right_boxes, robot, dir)
            continue
        if next in left_boxes or next in right_boxes:
            if move_boxes(next, dir):
                robot = next
            print_warehouse(walls, left_boxes, right_boxes, robot, dir)
            continue
        robot = next
        print_warehouse(walls, left_boxes, right_boxes, robot, dir)
        if dir == (-1, 0): quit()

    return sum_gps(left_boxes)


def widen(walls:set[Coord], boxes:set[Coord]):
    """Widen walls and boxes."""
    new_walls: set[Coord] = set()
    left_boxes: set[Coord] = set()
    right_boxes: set[Coord] = set()
    for row, col in walls:
        new_walls.add((row, 2*col))
        new_walls.add((row, 2*col+1))
    for row, col in boxes:
        left_boxes.add((row, 2*col))
        right_boxes.add((row, 2*col+1))
    return new_walls, left_boxes, right_boxes



def main():
    """Parse input file, pass to puzzle solvers."""
    direction = {
        '^': (-1, 0),
        'v': (1, 0),
        '<': (0, -1),
        '>': (0, 1),
    }

    walls: set[Coord] = set()
    boxes: set[Coord] = set()
    directions: list[Coord] = []
    row = 0
    parse = 'warehouse'
    start = (0, 0)
    for line in sys.stdin:
        line = line.strip()
        if not line:
            parse = 'directions'
            continue
        if parse == 'warehouse':
            for col, char in enumerate(line):
                if char == '#':
                    walls.add((row, col))
                elif char == 'O':
                    boxes.add((row, col))
                elif char == '@':
                    start = (row, col)
            row += 1
        elif parse == 'directions':
            for char in line:
                directions.append(direction[char])


    # print('part_one', part_one(walls.copy(), boxes.copy(), directions, start))

    walls, left_boxes, right_boxes = widen(walls, boxes)
    r,c = start

    print('part_two', part_two(walls, left_boxes, right_boxes, directions, (r, 2*c)))


main()
