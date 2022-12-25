const { snafu2dec, dec2snafu } = require('./25-snafu')

describe('snafu2dec', () => {
  it.each([
    /* eslint-disable */
    [  '1',  1 ],
    [  '2',  2 ],
    [ '1=',  3 ],
    [ '1-',  4 ],
    [ '10',  5 ],
    [ '11',  6 ],
    [ '12',  7 ],
    [ '2=',  8 ],
    [ '2-',  9 ],
    [ '20', 10 ],
    ['1=0', 15 ],
    ['1-0', 20 ],
    /* eslint-enable */
  ])('parses basic %s into %d', (input, expected) => {
    expect(snafu2dec(input)).toBe(expected)
  })

  it.each([
    /* eslint-disable */
    [       '1=11-2',      2022 ],
    [      '1-0---0',     12345 ],
    ['1121-1110-1=0', 314159265 ],
    /* eslint-enable */
  ])('parses %s into %s', (input, expected) => {
    expect(snafu2dec(input)).toBe(expected)
  })

  it.each([
    /* eslint-disable */
    ['1=-0-2', 1747],
    [ '12111',  906],
    [  '2=0=',  198],
    [    '21',   11],
    [  '2=01',  201],
    [   '111',   31],
    [ '20012', 1257],
    [   '112',   32],
    [ '1=-1=',  353],
    [  '1-12',  107],
    [    '12',    7],
    [    '1=',    3],
    [   '122',   37],
    /* eslint-enable */
  ])('parses %s into %s', (input, expected) => {
    expect(snafu2dec(input)).toBe(expected)
  })
})

describe('dec2snafu', () => {
  it.each([
    /* eslint-disable */
    [  1,   '1' ],
    [  2,   '2' ],
    [  3,  '1=' ],
    [  4,  '1-' ],
    [  5,  '10' ],
    [  6,  '11' ],
    [  7,  '12' ],
    [  8,  '2=' ],
    [  9,  '2-' ],
    [ 10,  '20' ],
    [ 15, '1=0' ],
    [ 20, '1-0' ],
    /* eslint-enable */
  ])('parses basic %d into %s', (input, expected) => {
    expect(dec2snafu(input)).toBe(expected)
  })

  it.each([
    /* eslint-disable */
    [      2022,        '1=11-2' ],
    [     12345,       '1-0---0' ],
    [ 314159265, '1121-1110-1=0' ],
    /* eslint-enable */
  ])('parses %d into %s', (input, expected) => {
    expect(dec2snafu(input)).toBe(expected)
  })

  it.each([
    /* eslint-disable */
    [ 1747,  '1=-0-2' ],
    [  906,   '12111' ],
    [  198,    '2=0=' ],
    [   11,      '21' ],
    [  201,    '2=01' ],
    [   31,     '111' ],
    [ 1257,   '20012' ],
    [   32,     '112' ],
    [  353,   '1=-1=' ],
    [  107,    '1-12' ],
    [    7,      '12' ],
    [    3,      '1=' ],
    [   37,     '122' ],
    /* eslint-enable */
  ])('parses %d into %s', (input, expected) => {
    expect(dec2snafu(input)).toBe(expected)
  })
})
