"""2017 Day 18: Duet"""
import fileinput
import re
from collections import deque, defaultdict

re_set = re.compile(r'set (.+) (.+)')
re_add = re.compile(r'add (.+) (.+)')
re_mul = re.compile(r'mul (.+) (.+)')
re_mod = re.compile(r'mod (.+) (.+)')
re_snd = re.compile(r'snd (.+)')
re_rcv = re.compile(r'rcv (.+)')
re_jgz = re.compile(r'jgz (.+) (.+)')

def solve(lines: list[str]):
    """Solve the puzzle."""
    registers:dict[str,int] = defaultdict(int)
    idx = 0
    last_sound = 0

    def valueof(x: str) -> int:
        try:
            return int(x)
        except ValueError:
            return registers[x]

    while True:
        line = lines[idx]
        # print(f'idx {idx} line: {line} registers: {registers}')
        if m := re_set.match(line):
            x, y = m.groups()
            registers[x] = valueof(y)
            idx += 1
            continue
        elif m := re_add.match(line):
            x, y = m.groups()
            registers[x] += valueof(y)
            idx += 1
            continue
        elif m := re_mul.match(line):
            x, y = m.groups()
            registers[x] *= valueof(y)
            idx += 1
            continue
        elif m := re_mod.match(line):
            x, y = m.groups()
            registers[x] %= valueof(y)
            idx += 1
            continue
        elif m := re_snd.match(line):
            x = m.group(1)
            try:
                last_sound = int(x)
            except ValueError:
                last_sound = registers[x]
            # print(f'snd {last_sound}')
            idx += 1
            continue
        elif m := re_rcv.match(line):
            x = m.group(1)
            if registers[x] != 0:
                return last_sound
            idx += 1
            continue
        elif m := re_jgz.match(line):
            x, y = m.groups()
            x_val = valueof(x)
            y_val = valueof(y)
            if x_val > 0:
                idx += y_val
            else:
                idx += 1
        else:
            raise ValueError(f'Unrecognized line: {line}')


class Partner:
    """A Partner program to run the duet."""
    def __init__(self, program_id: int, lines: list[str]):
        self.pid = program_id
        self.lines = lines
        self.registers:dict[str,int] = defaultdict(int)
        self.registers['p'] = program_id
        self.idx = 0
        self.recv_queue:deque[int] = deque()
        self.send_count = 0

    def set_send_queue(self, queue: deque[int]):
        """Set the send queue to the given queue."""
        self.send_queue = queue

    def run(self):
        """Run the program until it needs to wait for input."""
        def valueof(x: str) -> int:
            try:
                return int(x)
            except ValueError:
                return self.registers[x]

        while True:
            line = self.lines[self.idx]
            if m := re_set.match(line):
                x, y = m.groups()
                self.registers[x] = valueof(y)
                self.idx += 1
                continue
            elif m := re_add.match(line):
                x, y = m.groups()
                self.registers[x] += valueof(y)
                self.idx += 1
                continue
            elif m := re_mul.match(line):
                x, y = m.groups()
                self.registers[x] *= valueof(y)
                self.idx += 1
                continue
            elif m := re_mod.match(line):
                x, y = m.groups()
                self.registers[x] %= valueof(y)
                self.idx += 1
                continue
            elif m := re_snd.match(line):
                x = m.group(1)
                last_sound = None
                try:
                    last_sound = int(x)
                except ValueError:
                    last_sound = self.registers[x]
                self.send_queue.append(last_sound)
                self.send_count += 1
                self.idx += 1
                continue
            elif m := re_rcv.match(line):
                x = m.group(1)
                if self.recv_queue:
                    self.registers[x] = self.recv_queue.popleft()
                else:
                    return

                self.idx += 1
                continue
            elif m := re_jgz.match(line):
                x, y = m.groups()
                x_val = valueof(x)
                y_val = valueof(y)
                if x_val > 0:
                    self.idx += y_val
                else:
                    self.idx += 1
            else:
                raise ValueError(f'Unrecognized line: {line}')


def part_one(lines: list[str]):
    """Solution to part one."""
    return solve(lines)


def part_two(lines: list[str]):
    """Solution to part two."""
    p0 = Partner(0, lines)
    p1 = Partner(1, lines)
    p0.set_send_queue(p1.recv_queue)
    p1.set_send_queue(p0.recv_queue)

    while True:
        p0.run()
        p1.run()
        if not p0.recv_queue and not p1.recv_queue:
            break

    return p1.send_count


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = [line.strip() for line in fileinput.input()]

    print('part_one', part_one(lines))
    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()
