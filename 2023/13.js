const R = require('ramda')
const readline = require('readline')
const { flip } = require('../utils.js')
const rl = readline.createInterface({ input: process.stdin })
const sum = (a, b) => a + b

/**
 * @typedef {string[]} Pattern
 */

function main () {
  /** @type {Pattern[]} */
  const patterns = []

  /** @type {Pattern} */
  let current = []

  rl.on('line', line => {
    if (line === '') {
      patterns.push(current)
      current = []
      return
    }
    current.push(line)
  })

  rl.on('close', () => {
    patterns.push(current)
    console.log('partOne', partOne(patterns))
    console.log('partTwo', partTwo(patterns))
  })
}

main()

/** @type {(pattern:Pattern) => Pattern} */
const flipMirror = R.pipe(
  R.map(R.split('')),
  flip,
  R.map(R.join('')),
)

/** @type {(patterns: Pattern[]) => number} */
function partOne (patterns) {
  /** @type {(pattern: Pattern, row: number) => boolean} */
  const confirmMirror = (pattern, row) => R.zipWith(
    R.equals,
    pattern.slice(0, row).reverse(),
    pattern.slice(row),
  ).reduce(R.and)

  /** @type {(pattern: Pattern) => number} */
  const findMirror = pattern => {
    for (let row = 1; row < pattern.length; row += 1) {
      if (pattern[row] === pattern[row - 1] && confirmMirror(pattern, row)) {
        return row
      }
    }
    return 0
  }

  /** @type {(pattern: Pattern) => number} */
  const findBothMirrors = pattern =>
    100 * findMirror(pattern) + findMirror(flipMirror(pattern))

  return patterns.map(findBothMirrors).reduce(sum)
}

/** @type {(patterns: Pattern[]) => number} */
function partTwo (patterns) {
  const numDifferent = (reflection, line) => {
    const eq = (a, b) => a === b ? 0 : 1
    return R.zipWith(eq, reflection, line).reduce(sum)
  }

  const possibleEqual = (reflection, line) => {
    return numDifferent(reflection, line) <= 1
  }

  /** @type {(pattern: Pattern, row: number) => boolean} */
  const confirmMirror = (pattern, row) => R.zipWith(
    numDifferent,
    pattern.slice(0, row).reverse(),
    pattern.slice(row),
  ).reduce(sum) === 1

  /** @type {(pattern: Pattern) => number} */
  const findMirror = pattern => {
    for (let row = 1; row < pattern.length; row += 1) {
      if (possibleEqual(pattern[row], pattern[row - 1]) && confirmMirror(pattern, row)) {
        return row
      }
    }
    return 0
  }

  /** @type {(pattern: Pattern) => number} */
  const findBothMirrors = pattern =>
    100 * findMirror(pattern) + findMirror(flipMirror(pattern))

  // too low: 37861
  return patterns.map(findBothMirrors).reduce(sum)
}
