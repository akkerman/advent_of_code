
from util_09 import remove_garbage, score_groups, take_garbage

def test_remove_garbage():
    assert remove_garbage('<>') == ''
    assert remove_garbage('<random characters>') == ''
    assert remove_garbage('<<<<>') == ''
    assert remove_garbage('<{!>}>') == ''
    assert remove_garbage('<!!>') == ''
    assert remove_garbage('<!!!>>') == ''
    assert remove_garbage('<{o"i!a,<{i<a>') == ''
    assert remove_garbage('{<a>,<a>,<a>,<a>}') == '{,,,}'
    assert remove_garbage('{{<ab>},{<ab>},{<ab>},{<ab>}}') == '{{},{},{},{}}'
    assert remove_garbage('{{<!!>},{<!!>},{<!!>},{<!!>}}') == '{{},{},{},{}}'
    assert remove_garbage('{{<a!>},{<a!>},{<a!>},{<ab>}}') == '{{}}'

def test_score_groups():
    assert score_groups('{}') == 1
    assert score_groups('{{{}}}') == 6
    assert score_groups('{{},{}}') == 5

def test_part_one():
    def p1(line:str) -> int:
        return score_groups(remove_garbage(line))
    assert(p1('{}')) == 1
    assert(p1('{{{}}}')) == 6
    assert(p1('{{},{}}')) == 5
    assert(p1('{{{},{},{{}}}}')) == 16
    assert(p1('{<a>,<a>,<a>,<a>}')) == 1
    assert(p1('{{<ab>},{<ab>},{<ab>},{<ab>}}')) == 9
    assert(p1('{{<!!>},{<!!>},{<!!>},{<!!>}}')) == 9
    assert(p1('{{<a!>},{<a!>},{<a!>},{<ab>}}')) == 3

def test_take_garbage():
    assert(take_garbage('<>')) == ''
    assert(take_garbage('<random characters>')) == 'random characters'
    assert(take_garbage('<<<<>')) == '<<<'
    assert(take_garbage('<{!>}>')) == '{}'
    assert(take_garbage('<!!>')) == ''
    assert(take_garbage('<!!!>>')) == ''
    assert(take_garbage('<{o"i!a,<{i<a>')) == '{o"i,<{i<a'

