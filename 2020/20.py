# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


class Tile:
    def __init__(self, tile_id, content):
        self.tile_id = int(tile_id)
        self.content = content
        self.matches_with = []

        self.top = content[0]
        self.bottom = content[-1]

        self.right = ''
        self.left = ''

        for line in content:
            self.left += line[0]
            self.right += line[-1]

    def __repr__(self):
        return str((self.tile_id, self.content))

    def sides_matching(self, other):
        for side in (self.right, self.left, self.bottom, self.top):
            for other_side in (other.right, other.left, other.bottom, other.top):
                if other_side in (side, side[::-1]):
                    self.matches_with.append(other.tile_id)
                    return True
        return False


def part_one(tiles):
    """ part one """
    for tile in tiles.values():
        for other in tiles.values():
            if tile.tile_id == other.tile_id:
                continue
            tile.sides_matching(other)

    result = 1
    for tile in tiles.values():
        if len(tile.matches_with) == 2:
            result *= int(tile.tile_id)

    return result

def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    tiles = {}
    tid = ''
    tile = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        if line == '':
            tiles[tid] = Tile(tid, tile)
            tid = ''
            tile = []
            continue

        if tid == '':
            tid = line.split(' ')[1].replace(':', '')
            continue

        tile.append(line)


    print('part_one', part_one(tiles))

    print('part_two', part_two(tiles))


main()
