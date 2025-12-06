"""2017 Day 25: The Halting Problem"""
import fileinput
import re
from typing import Literal


StateId = str
Value = bool
State = tuple[StateId, Value]
Dir = Literal[-1, 1]

class Machine:
    """Turing machine simulator."""
    def __init__(self, start_state_id:str):
        self.tape = set[int]() # positions on the tape with value 1
        self.cursor: int = 0
        self.state_id: str = start_state_id
        self.transitions: dict[State, tuple[Value, Dir, StateId]] = {}

    def step(self):
        """Perform a single step of the Turing machine."""
        current_value = self.cursor in self.tape

        state: State = (self.state_id, current_value)
        new_value, dir, to_state = self.transitions[state]
        if new_value:
            self.tape.add(self.cursor)
        else:
            self.tape.discard(self.cursor)

        self.cursor += dir
        self.state_id = to_state

    def run(self, steps:int):
        """Run the Turing machine for a number of steps."""
        self.tape = set[int]()
        for _ in range(steps):
            self.step()

    def checksum(self):
        """Calculate the checksum of the tape."""
        return len(self.tape)

    def add_transition(self, from_state: StateId, current_value: Value, new_value:Value, dir: Dir, to_state: StateId):
        """Add a transition to the Turing machine."""
        self.transitions[(from_state, current_value)] = (new_value, dir, to_state)


re_start_state = re.compile(r'Begin in state (.+)\.')
re_steps = re.compile(r'Perform a diagnostic checksum after (\d+) steps\.')

re_from_state = re.compile(r'In state (.+):')
re_current_value = re.compile(r'If the current value is (0|1):')

re_new_value = re.compile(r'- Write the value (0|1)\.')
re_dir = re.compile(r'- Move one slot to the (right|left)\.')
re_to_state = re.compile(r'- Continue with state (.+)\.')

def main():
    """Parse input file, pass to puzzle solvers."""
    machine: Machine | None = None
    steps = 0

    from_state: StateId | None = None
    current_value: Value | None = None
    new_value: Value | None = None
    dir: Dir | None = None
    to_state: StateId | None = None

    for line in fileinput.input():
        line = line.strip()
        if not line:
            continue

        if m := re_start_state.match(line):
            start_state = m.group(1)
            machine = Machine(start_state)
        elif m := re_steps.match(line):
            steps = int(m.group(1))
        elif m := re_from_state.match(line):
            from_state = m.group(1)
        elif m := re_current_value.match(line):
            current_value = True if m.group(1) == '1' else False
        elif m := re_new_value.match(line):
            new_value = True if m.group(1) == '1' else False
        elif m := re_dir.match(line):
            dir = 1 if m.group(1) == 'right' else -1
        elif m := re_to_state.match(line):
            to_state = m.group(1)

            assert machine is not None

            assert from_state is not None
            assert current_value is not None

            assert new_value is not None
            assert dir is not None
            assert to_state is not None

            machine.add_transition(from_state, current_value, new_value, dir, to_state)

            current_value = None
            new_value = None
            dir = None
            to_state = None
        else:
            raise ValueError(f'Unrecognized line: {line}')
        
    assert machine is not None
    machine.run(steps)
    print('part_one', machine.checksum())



if __name__ == '__main__':
    main()
