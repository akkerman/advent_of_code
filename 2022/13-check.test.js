const check = require('./13-check.js')

describe('check', () => {
  it.each([
    ['1', true, [1, 1, 3, 1, 1], [1, 1, 5, 1, 1]],
    ['2', true, [[1], [2, 3, 4]], [[1], 4]],
    ['3', false, [9], [[8, 7, 6]]],
    ['4', true, [[4, 4], 4, 4], [[4, 4], 4, 4, 4]],
    ['5', false, [7, 7, 7, 7], [7, 7, 7]],
    ['6', true, [], [3]],
    ['7', false, [[[]]], [[]]],
    ['8', false, [1, [2, [3, [4, [5, 6, 7]]]], 8, 9], [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]],
  ])('example: checks %i as %s', (_, expected, left, right) => {
    expect(check(left, right)).toEqual(expected)
  })
})
