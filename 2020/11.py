# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def seat_set(lines):
    seats = set()
    for row, line in enumerate(lines):
        for col, s in enumerate(line):
            if s == 'L':
                seats.add((row, col))

    return seats


offsets = [
    (-1, -1), (-1, 0), (-1, 1),
    (0, -1), (0, 1),
    (1, -1), (1, 0), (1, 1),
    ]


def adjacent_seats(seat):
    r, c = seat
    return {(r+dr, c+dc) for dr, dc in offsets}

def make_visible_seats(seats):
    max_row = max([r for (r, _) in seats])
    max_col = max([c for (_, c) in seats])

    def visible_seat(seat):
        visible = set()
        for dr, dc in offsets:
            r, c = seat
            while 0 <= r <= max_row and 0 <= c <= max_col:
                r += dr
                c += dc
                nb = (r, c)
                if nb in seats:
                    visible.add(nb)
                    break

        return visible

    return visible_seat


def part_one(seats):
    """ part one """
    occupied = set()
    rounds = 0
    while True:
        rounds += 1
        new_occupied = set(occupied)
        for seat in seats:
            possible_seats = seats & adjacent_seats(seat)

            if seat in occupied:
                if len(possible_seats & occupied) >= 4:
                    new_occupied.remove(seat)
            else:
                free_seats = possible_seats - occupied
                if free_seats == possible_seats:
                    new_occupied.add(seat)

        if occupied == new_occupied:
            return len(occupied)

        occupied = new_occupied



def part_two(seats):
    """ part two """
    occupied = set()
    rounds = 0
    visible_seats = make_visible_seats(seats)
    while True:
        rounds += 1
        new_occupied = set(occupied)
        for seat in seats:
            possible_seats = seats & visible_seats(seat)

            if seat in occupied:
                if len(possible_seats & occupied) >= 5:
                    new_occupied.remove(seat)
            else:
                free_seats = possible_seats - occupied
                if free_seats == possible_seats:
                    new_occupied.add(seat)

        if occupied == new_occupied:
            return len(occupied)

        occupied = new_occupied


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    print('part_one', part_one(seat_set(lines)))

    print('part_two', part_two(seat_set(lines)))


main()
