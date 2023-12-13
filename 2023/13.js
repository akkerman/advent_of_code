const R = require('ramda')
const { flip } = require('../utils')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap // eslint-disable-line 
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

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

/** @type {(patterns: Pattern[]) => number */
function partOne (patterns) {
  /** @type {(pattern:Pattern) => Pattern} */
  const flipMirror = R.pipe(
    R.map(R.split('')),
    flip,
    R.map(R.join('')),
  )

  /** @type {(pattern: Pattern, row: number) => boolean} */
  const confirmMirror = (pattern, row) => {
    const before = pattern.slice(0, row).reverse()
    const after = pattern.slice(row)

    return R.zipWith(R.equals, before, after).reduce(R.and)
  }

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

function partTwo (lines) {
  return 'todo'
}
