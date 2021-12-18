const {
  add, explode, parse, split, reduce,
} = require('./18-Snailfish-lib.js')

describe('parse', function () {
  it.each([
    { input: [[[[[9, 8], 1], 2], 3], 4] },
    { input: [7, [6, [5, [4, [3, 2]]]]] },
    { input: [[6, [5, [4, [3, 2]]]], 1] },
    { input: [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]] },
    { input: [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]] },
  ])('parse pair', (input) => {
    expect(parse(input).show()).toEqual(input)
  })

  describe('simple tuple', () => {
    let tuple
    beforeEach(() => {
      tuple = parse([1, 2])
    })

    it('fst.value = 1', () => { expect(tuple.fst.value).toBe(1) })
    it('snd.value = 2', () => { expect(tuple.snd.value).toBe(2) })
    it('fst.parent = tuple', () => { expect(tuple.fst.parent).toBe(tuple) })
    it('snd.parent = tuple', () => { expect(tuple.snd.parent).toBe(tuple) })
  })
})

describe('to the right', function () {
  it('snd is right of fst', () => {
    const pair = parse([[9, 8], 1])

    expect(pair.fst.right()).toBe(pair.snd)
  })
  it('fst is left of snd', () => {
    const pair = parse([9, [2, 1]])

    expect(pair.snd.left()).toBe(pair.fst)
  })

  it('works in highter hierargy', () => {
    const root = parse([[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]])
    const pair = root.fst.snd.snd.snd
    expect(pair.show()).toEqual([7, 3])
    expect(pair.right().show()).toEqual(6)
  })
})

describe('add', () => {
  let left
  let right
  let fst
  let snd
  let result
  beforeEach(() => {
    left = [1, 2]
    right = [[3, 4], 5]
    fst = parse(left)
    snd = parse(right)
    result = add(fst, snd)
  })
  it('pairs left and right', () => {
    expect(result.show()).toEqual([left, right]) // [[1, 2],[[3, 4], 5]]
  })
})

describe('explode', () => {
  it.each([
    { input: [[[[[9, 8], 1], 2], 3], 4], becomes: [[[[0, 9], 2], 3], 4] },
    { input: [7, [6, [5, [4, [3, 2]]]]], becomes: [7, [6, [5, [7, 0]]]] },
    { input: [[6, [5, [4, [3, 2]]]], 1], becomes: [[6, [5, [7, 0]]], 3] },
    { input: [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]], becomes: [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]] },
    { input: [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]], becomes: [[3, [2, [8, 0]]], [9, [5, [7, 0]]]] },
    { input: [[[[0, 7], 4], [7, [[8, 4], 9]]], [1, 1]], becomes: [[[[0, 7], 4], [15, [0, 13]]], [1, 1]] },
  ])('explodes the result', ({ input, becomes }) => {
    const pair = parse(input)
    const result = explode(pair)

    expect(result).toEqual(true)
    expect(pair.show()).toEqual(becomes)
  })
})

describe('split', () => {
  it.each([
    { input: 10, becomes: [5, 5] },
    { input: 11, becomes: [5, 6] },
    { input: 12, becomes: [6, 6] },
  ])('splits stuff', ({ input, becomes }) => {
    const pair = parse([1, input])
    const result = split(pair)

    expect(result).toEqual(true)
    expect(pair.snd.show()).toEqual(becomes)
  })
  it('does not split multiple', () => {
    const pair = parse([1, [10, 20]])
    const result = split(pair)

    expect(result).toEqual(true)
    expect(pair.snd.show()).toEqual([[5, 5], 20])
  })
  it('does not split smaller than 10', () => {
    const pair = parse([1, 9])
    const result = split(pair)

    expect(result).toEqual(false)
  })
})

describe('magnitude', function () {
  it.each([
    { input: [[1, 2], [[3, 4], 5]], becomes: 143 },
    { input: [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]], becomes: 1384 },
    { input: [[[[1, 1], [2, 2]], [3, 3]], [4, 4]], becomes: 445 },
    { input: [[[[3, 0], [5, 3]], [4, 4]], [5, 5]], becomes: 791 },
    { input: [[[[5, 0], [7, 4]], [5, 5]], [6, 6]], becomes: 1137 },
    { input: [[[[8, 7], [7, 7]], [[8, 6], [7, 7]]], [[[0, 7], [6, 6]], [8, 7]]], becomes: 3488 },

  ])('magnitude',
    ({ input, becomes }) => {
      const pair = parse(input)
      expect(pair.magnitude()).toBe(becomes)
    },
  )
})

describe('add reduce', function () {
  it('add two', () => {
    const pair = add(
      parse([[[[4, 3], 4], 4], [7, [[8, 4], 9]]]),
      parse([1, 1]),
    )

    const sum = [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]]

    expect(reduce(pair).show()).toEqual(sum)
  })
})

describe('partOne - make and verify example homework assignment', function () {
  it('has magnitude 4140', () => {
    const pairs = [
      [[[0, [5, 8]], [[1, 7], [9, 6]]], [[4, [1, 2]], [[1, 4], 2]]],
      [[[5, [2, 8]], 4], [5, [[9, 9], 0]]],
      [6, [[[6, 2], [5, 6]], [[7, 6], [4, 7]]]],
      [[[6, [0, 7]], [0, 9]], [4, [9, [9, 0]]]],
      [[[7, [6, 4]], [3, [1, 3]]], [[[5, 5], 1], 9]],
      [[6, [[7, 3], [3, 2]]], [[[3, 8], [5, 7]], 4]],
      [[[[5, 4], [7, 7]], 8], [[8, 3], 8]],
      [[9, 3], [[9, 9], [6, [4, 9]]]],
      [[2, [[7, 7], 7]], [[5, 8], [[9, 3], [0, 2]]]],
      [[[[5, 2], 5], [8, [3, 7]]], [[5, [7, 5]], [4, 4]]],
    ].map(parse)

    let result

    for (const pair of pairs) {
      if (!result) {
        result = pair
        continue
      }
      result = reduce(add(result, pair))
    }
    const mag = result.magnitude()

    expect(mag).toEqual(4140)
  })
})

describe('partTwo - largest magnitude', function () {
  test('maxMagnitude is 3993', () => {
    const lines = [
      [[[0, [5, 8]], [[1, 7], [9, 6]]], [[4, [1, 2]], [[1, 4], 2]]],
      [[[5, [2, 8]], 4], [5, [[9, 9], 0]]],
      [6, [[[6, 2], [5, 6]], [[7, 6], [4, 7]]]],
      [[[6, [0, 7]], [0, 9]], [4, [9, [9, 0]]]],
      [[[7, [6, 4]], [3, [1, 3]]], [[[5, 5], 1], 9]],
      [[6, [[7, 3], [3, 2]]], [[[3, 8], [5, 7]], 4]],
      [[[[5, 4], [7, 7]], 8], [[8, 3], 8]],
      [[9, 3], [[9, 9], [6, [4, 9]]]],
      [[2, [[7, 7], 7]], [[5, 8], [[9, 3], [0, 2]]]],
      [[[[5, 2], 5], [8, [3, 7]]], [[5, [7, 5]], [4, 4]]],
    ]

    let result
    let maxMagnitude = 0

    for (const line1 of lines) {
      for (const line2 of lines) {
        result = reduce(add(parse(line1), parse(line2)))
        maxMagnitude = Math.max(maxMagnitude, result.magnitude())
      }
    }

    expect(maxMagnitude).toEqual(3993)
  })
})
