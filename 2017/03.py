# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import numpy as np
from typing import List

def get_corner(n:int):
    m = int(n**0.5)
    if m % 2 == 0:
        m += 1
    while True:
        k = m**2
        if k >= n:
            return m, k
        m += 2



def part_one(target:int):
    """ part one """
    m, k = get_corner(target)

    corners: List[int] = [k, k-m+1, k-2*m+2, k-3*m+3, (m-2)**2]
    if target in corners:
        return m

    centers: List[int] = []
    s = corners[0]
    for c in corners:
        centers.append(int((s+c)/2))
        s = c

    if target in centers:
        return m // 2


    min_dist = m
    for c in centers:
        min_dist = min(min_dist, abs(c - target))

    return m//2 + min_dist


def part_two(target:int):
    """ part two """

    # see https://oeis.org/A141481
    # translated to python with chatgpt.

    m = 7
    h = 2 * m - 1
    A = np.zeros((h, h), dtype=int)
    A[m - 1, m - 1] = 1

    # Movement directions for neighbors (8 directions)
    T = [
        [1, 0], [1, -1], [0, -1], [-1, -1], 
        [-1, 0], [-1, 1], [0, 1], [1, 1]
    ]

    for n in range(1, (h - 2) ** 2):
        g = int(n**0.5)
        r = (g + g % 2) // 2
        q = 4 * r**2
        d = n - q
        
        # Calculate j, k based on n
        if n <= q - 2 * r:
            j, k = d + 3 * r, r
        elif n <= q:
            j, k = r, -d - r
        elif n <= q + 2 * r:
            j, k = r - d, -r
        else:
            j, k = -r, d - 3 * r
        
        # Convert coordinates to matrix indices
        j += m - 1
        k += m - 1
        
        # Sum of neighbors
        s = 0
        for c in range(8):
            v_j, v_k = j + T[c][0], k + T[c][1]
            if 0 <= v_j < h and 0 <= v_k < h:  # Check bounds
                s += A[v_j, v_k]
        
        # Assign value and print
        A[j, k] = s
        # print(f"{s}, ", end="")
        if s > target:
            return s


def main():
    """ main """
    lines: List[int] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        
        lines.append(int(line))

    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines[0]))


main()
