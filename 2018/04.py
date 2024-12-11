"""Day 4: Repose Record."""
import sys
from typing import Counter
from collections import defaultdict, Counter
import re

GUARD_PATTERN = r'\[.+\] Guard #(\d+) begins shift$'
SLEEP_PATTERN = r'\[.+:(\d\d)] falls asleep$'
WAKE_PATTERN = r'\[.+:(\d\d)] wakes up$'

def count_sleep(log: list[str]) -> defaultdict[str, Counter[int]]:
    """ Count the frequency of each minute a guard is asleep."""
    guards: defaultdict[str, Counter[int]] = defaultdict(Counter)

    guard = ""
    start = -1
    end = -1
    for line in log:
        if 'Guard' in line:
            guard: str = re.findall(GUARD_PATTERN, line)[0]  # type: ignore
            continue
        if 'falls asleep' in line:
            start: int = int(re.findall(SLEEP_PATTERN, line)[0]) # type: ignore
            continue
        if 'wakes up' in line:
            end: int = int(re.findall(WAKE_PATTERN, line)[0]) # type: ignore
            guards[guard].update(range(start, end))

    return guards


def most_common_minute(minutes: Counter[int]) -> int:
    return minutes.most_common(1)[0][0]

def part_one(log: list[str]) -> int:
    """Search for the guard who sleeps the most on the job."""
    guards = count_sleep(log)

    max_guard = ""
    max_mins_asleep = 0 # total minutes asleep
    for guard, minutes in guards.items():
        mins_asleep = sum(minutes.values())
        if mins_asleep > max_mins_asleep:
            max_mins_asleep = mins_asleep
            max_guard = guard

    return int(max_guard) * most_common_minute(guards[max_guard])


def part_two(log: list[str]) -> int:
    """Search for the guard who sleeps the most during a single minute."""
    guards = count_sleep(log)
    
    max_guard = ""
    max_mins_asleep = 0 # max of any one minute asleep
    for guard, minutes in guards.items():
        mins_asleep = minutes.most_common(1)[0][1]
        if mins_asleep > max_mins_asleep:
            max_mins_asleep = mins_asleep
            max_guard = guard

    return int(max_guard) * most_common_minute(guards[max_guard])


def main():
    """Parse input file, pass to puzzle solvers."""
    lines: list[str] = []
    for line in sys.stdin:
        line = line.strip()
        lines.append(line)

    print('part_one', part_one(sorted(lines)))

    print('part_two', part_two(sorted(lines)))


main()
