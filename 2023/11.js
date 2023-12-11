const { findOccurences, manhattanDistance } = require('../utils.js')

/**
 * @typedef {string[]} Universe
 * @typedef {import('../utils.js').Coord} Coord
 */

const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const sum = (a, b) => a + b

const GALAXY = '#'

function main () {
  const universe = []

  rl.on('line', line => {
    universe.push(line)
  })

  rl.on('close', () => {
    console.log('partOne', partOne(universe))
    console.log('partTwo', partTwo(universe))
  })
}

main()

/** @type {(universe: Universe) => Coord[]} */
function extractGalaxyCoords (universe) {
  const coords = []
  for (let r = 0; r < universe.length; r += 1) {
    coords.push(
      ...findOccurences(universe[r], GALAXY).map(c => [r, c]),
    )
  }
  return coords
}

/** @type {(universe: Universe) => number[]} */
const determineEmptyRowIds = universe => universe.reduce(
  (rows, line, idx) => (line.includes(GALAXY)) ? rows : [...rows, idx],
  [],
)

/** @type {(universe: Universe) => number[]} */
const determineEmptyColumnIds = universe => {
  const emptyCols = []
  for (let c = 0; c < universe[0].length; c += 1) {
    let isEmpty = true
    for (let r = 0; r < universe.length; r += 1) {
      if (universe[r][c] === GALAXY) {
        isEmpty = false
        break
      }
    }
    if (isEmpty) {
      emptyCols.push(c)
    }
  }
  return emptyCols
}

/** @type {(universe: Universe, factor:number) => number} */
function solve (universe, factor) {
  const shift = factor - 1

  const emptyRows = determineEmptyRowIds(universe)
  const emptyCols = determineEmptyColumnIds(universe)

  const shiftGalaxy = ([r, c]) => [
    r + emptyRows.filter(id => id < r).length * shift,
    c + emptyCols.filter(id => id < c).length * shift,
  ]

  const coords = extractGalaxyCoords(universe).map(shiftGalaxy)

  const distances = []
  for (let i = 0; i < coords.length; i += 1) {
    const start = coords[i]
    for (const end of coords.slice(i + 1)) {
      distances.push(manhattanDistance(start, end))
    }
  }

  return distances.reduce(sum)
}

/** @type {(universe: Universe) => number} */
function partOne (universe) {
  return solve(universe, 2)
}

/** @type {(universe: Universe) => number} */
function partTwo (universe) {
  return solve(universe, 1_000_000)
}
