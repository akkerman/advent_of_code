"""2018 Day 10: The Stars Align"""
import fileinput
import re

Points = list[tuple[int, int, int, int]]  # (x, y, vx, vy)

def draw(points: Points):
    """Draw points to console."""
    coords = {(x,y) for x,y,xv,yv in points}
    min_x = min(p[0] for p in coords)
    max_x = max(p[0] for p in coords)
    min_y = min(p[1] for p in coords)
    max_y = max(p[1] for p in coords)

    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            if (x,y) in coords:
                print('#', end='')
            else:
                print(' ', end='')
        print()


def move(points: Points, time: int) -> Points:
    """Move points forward in time."""
    new_points: Points = []
    for (x, y, vx, vy) in points:
        new_points.append((x + vx * time, y + vy * time, vx, vy))
    return new_points


def solve(points: list[tuple[int, int, int, int]]):
    def bounding_box_area(time: int):
        """Calculate bounding box area at given time."""
        xs = [x + vx * time for (x, y, vx, vy) in points]
        ys = [y + vy * time for (x, y, vx, vy) in points]
        return (max(xs) - min(xs)) * (max(ys) - min(ys))

    time = 0
    area1 = bounding_box_area(time)

    while True:
        area2 = bounding_box_area(time + 1)
        if area2 > area1:
            break
        area1 = area2
        time += 1

    final_points = move(points, time)
    draw(final_points)
    print(f"Time: {time}")

re_points = re.compile(r'position=< ?(.+), ?(.+)> velocity=< ?(.+), ?(.+)>')

def main():
    """Parse input file, pass to puzzle solvers."""
    points: Points = []
    for line in fileinput.input():
        line = line.strip()
        m = re_points.match(line)
        assert m and len(m.groups()) == 4

        x, y, vx, vy = map(int, m.groups())
        points.append((int(x), int(y), int(vx), int(vy)))
        

    solve(points)

if __name__ == '__main__':
    main()
