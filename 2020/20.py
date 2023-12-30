# pylint: disable=missing-module-docstring,missing-function-docstring,missing-class-docstring
# pylint: disable=invalid-name
import sys
from itertools import chain
import numpy as np

R = 1
L = 2
B = 3
T = 4

SIDES = {
 1: 'right',
 2: 'left',
 3: 'bottom',
 4: 'top'
}

class Tile:
    def __init__(self, tile_id, content):
        self.tile_id = int(tile_id)
        self.content = []
        self.matches_with = set()

        self.right = []
        self.left = []
        self.bottom = []
        self.top = []

        self.set_content(content)

    def set_content(self, content):
        self.content = content
        self.set_sides()

    def set_sides(self):
        self.top = "".join(self.content[0])
        self.bottom = "".join(self.content[-1])

        self.right = ''
        self.left = ''

        for line in self.content:
            self.left += line[0]
            self.right += line[-1]

    def get_with_empty_border(self):
        arr = [[" ", *l] for l in self.content]
        return [
                [" "] * len(arr[0]),
                *arr,
                ]


    def get_without_border(self):
        return [l[1:-1] for l in self.content[1:-1]]

    # def __repr__(self):
    #     return str((self.tile_id, self.content))

    def __repr__(self):
        return str(self.tile_id)

    def is_corner(self):
        return len(self.matches_with) == 2

    def is_edge(self):
        return len(self.matches_with) <= 3 # corner is an edge

    def is_inner(self):
        return len(self.matches_with) == 4

    def sides_matching(self, other):
        for s, side in enumerate([self.right, self.left, self.bottom, self.top]):
            for other_side in (other.right, other.left, other.bottom, other.top):
                if other_side in (side, side[::-1]):
                    self.matches_with.add(other.tile_id)
                    return s+1
        return None

    def rotR(self):
        """ positive k turns counter clockwise """
        self.set_content(np.rot90(self.content, -1))

    def rotL(self):
        """ positive k turns counter clockwise """
        self.set_content(np.rot90(self.content))

    def fliplr(self):
        """ flip left right """
        self.set_content(np.fliplr(self.content))

    def flipud(self):
        """ flip up down """
        self.set_content(np.flipud(self.content))

class Image:
    def __init__(self):
        self.image = []

    def append(self, row):
        self.image.append(row)

    def print(self):
        for row in self.image:
            # content = [t.get_without_border() for t in row]
            content = [t.get_with_empty_border() for t in row]
            st = list([list(chain(*l)) for l in zip(*content)])
            for line in ["".join(l) for l in st]:
                print(line)


    def orient_corner(self):
        corner = self.image[0][0]
        corner_side = corner.sides_matching(self.image[0][1])

        if corner_side == T:
            corner.rotR()
        if corner_side == B:
            corner.rotL()
        if corner_side == L:
            corner.fliplr()

        corner_side = corner.sides_matching(self.image[0][1])
        assert corner_side == R

        other_side = corner.sides_matching(self.image[1][0])
        if other_side != B:
            corner.flipud()

        other_side = corner.sides_matching(self.image[1][0])
        assert other_side == B

    def orient_first_row(self):
        for idx, tile in enumerate(self.image[0]):
            if idx == 0:
                continue

            side = tile.sides_matching(self.image[0][idx-1])

            if side == T:
                tile.rotL()
            if side == B:
                tile.rotR()
            if side == R:
                tile.fliplr()

            side = tile.sides_matching(self.image[0][idx-1])
            assert side == L

            side = tile.sides_matching(self.image[1][idx])

            if side != B:
                tile.flipud()

            side = tile.sides_matching(self.image[1][idx])
            assert side == B

    def orient_middle_rows(self):
        for r, row in enumerate(self.image):
            if r == 0:
                continue

            # first tile
            tile = self.image[r][0]
            side = tile.sides_matching(self.image[r-1][0])
            if side == B:
                tile.flipud()
            if side == R:
                tile.rotL()
            if side == L:
                tile.rotR()
            assert tile.sides_matching(self.image[r-1][0]) == T

            side = tile.sides_matching(self.image[r][1])
            if side == L:
                tile.fliplr()
            assert tile.sides_matching(self.image[r][1]) == R

            # other tiles
            for idx, tile in enumerate(row):
                if idx == 0:
                    continue
                side = tile.sides_matching(self.image[r-1][idx])
                if side == B:
                    tile.flipud()
                if side == R:
                    tile.rotL()
                if side == L:
                    tile.rotR()
                assert tile.sides_matching(self.image[r-1][idx]) == T


    def orient(self):
        self.orient_corner()
        self.orient_first_row()
        self.orient_middle_rows()

def part_one(tiles):
    """ part one """
    for tile in tiles.values():
        for other in tiles.values():
            if tile == other:
                continue
            tile.sides_matching(other)

    result = 1
    for tile in tiles.values():
        if len(tile.matches_with) == 2:
            result *= int(tile.tile_id)

    return result

def part_two(tiles):
    """ part two """

    placed = set()

    def place_first_row(topleft):
        row = []
        row.append(topleft)
        placed.add(topleft.tile_id)
        current = topleft
        while True:
            for tile_id in current.matches_with:
                nxt = tiles[str(tile_id)]
                if not nxt.is_edge():
                    continue
                if nxt.tile_id in placed:
                    continue

                placed.add(nxt.tile_id)
                row.append(nxt)
                current = nxt
                break

            if current in corners:
                break

        return row

    def place_next_row(row):
        below = []

        matches = set(row[0].matches_with) - placed
        assert len(matches) == 1
        tid = matches.pop()
        placed.add(tid)
        below.append(tiles[str(tid)])

        for tile in row[1:]:
            inter = tile.matches_with & below[-1].matches_with
            matches = inter - placed
            assert len(matches) == 1
            tid = matches.pop()
            placed.add(tid)
            below.append(tiles[str(tid)])

        return below


    ########################################################################
    image = Image()
    corners = [t for t in tiles.values() if t.is_corner()]
    row = place_first_row(corners[0])
    image.append(row)

    while len(placed) < len(tiles):
        row = place_next_row(row)
        image.append(row)

    image.orient()

    image.print()
    ########################################################################

    return 'todo'


def main():
    """ main """
    tiles = {}
    tid = ''
    content = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        if line == '':
            tiles[tid] = Tile(tid, content)
            tid = ''
            content = []
            continue

        if tid == '':
            tid = line.split(' ')[1].replace(':', '')
            continue

        content.append(list(line))
        # content.append(line)


    print('part_one', part_one(tiles))


    print('part_two', part_two(tiles))


main()
