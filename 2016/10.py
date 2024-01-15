# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re

bots = {}
outputs = {}

class Bot:
    """ microchips passing bot """
    def __init__(self, bot_id, low, high):
        self.id = bot_id
        self.values = []
        self.low = low
        self.high = high

    def receive(self, value):
        self.values.append(value)
        if len(self.values) < 2:
            return

        self.values.sort()
        if self.values == [17, 61]:
            print(f'Bot {self.id} handles the requested values {self.values}')

        target, num = self.low
        if target == 'output':
            outputs[num] = self.values[0]
        else:
            bots[num].receive(self.values[0])

        target, num = self.high
        if target == 'output':
            outputs[num] = self.values[1]
        else:
            bots[num].receive(self.values[1])


    def __repr__(self):
        return f'Bot({self.id}) has {self.values} for {self.low} and {self.high}'

def part_one(values):
    """ part one """
    for value, bot_id in values:
        bots[bot_id].receive(value)

    return 'todo'


def part_two():
    """ part two """
    return outputs['0'] * outputs['1'] * outputs['2']


def main():
    """ main """
    re_val = re.compile(r'value (\d+) goes to bot (\d+)')
    re_bot = re.compile(r'bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)')
    values = []

    for line in sys.stdin:
        line = line.replace('\n', '')
        m = re_val.match(line)
        if m:
            value, bot_id = m.groups()
            values.append((int(value), bot_id))
            continue

        m = re_bot.match(line)
        if m:
            bot_id, low_target, low_id, high_target, high_id = m.groups()
            bots[bot_id] = Bot(bot_id, (low_target, low_id), (high_target, high_id))
            continue

        assert False


    print('\npart_one')
    part_one(values)

    print('\npart_two', part_two())


main()
