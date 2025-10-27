"""Day 8: Memory Maneuver."""
import fileinput
from collections import deque

class Node:
    """Node in the license tree."""

    def __init__(self, children: list['Node'], metadata: list[int]):
        self.children = children
        self.metadata = metadata

    @classmethod
    def from_license(cls, license: deque[int]) -> 'Node':
        """Parse a node from the license deque."""
        num_children = license.popleft()
        num_metadata = license.popleft()
        children = [cls.from_license(license) for _ in range(num_children)]
        metadata = [license.popleft() for _ in range(num_metadata)]
        return cls(children, metadata)
    
    def sum_metadata(self) -> int:
        """Sum of all metadata entries in this node and its children."""
        return sum(self.metadata) + sum(child.sum_metadata() for child in self.children)

    def value(self) -> int:
        """Value of this node."""
        if not self.children:
            return sum(self.metadata)

        child_values = 0
        for i in self.metadata:
            child_index = i - 1
            if 0 <= child_index < len(self.children):
                child_values += self.children[child_index].value()

        return child_values


def main():
    """Parse input file, pass to puzzle solvers."""
    license: list[int] = []
    for line in fileinput.input():
        line = line.strip()
        license = list(map(int, line.split(' ')))
        break

    root = Node.from_license(deque(license))
    print('part_one', root.sum_metadata())

    print('part_two', root.value())


if __name__ == '__main__':
    main()
