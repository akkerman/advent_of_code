const R = require('ramda')
const readline = require('readline')
const { newCoordSet, findOccurences } = require('../utils.js')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

/**
 * @typedef {import('../utils').CoordSet} CoordSet
 * @typedef {import('../utils').Coord} Coord
 */

const ROCK = {
  round: 'O',
  cube: '#',
}

function index (lines) {
  const roundRocks = newCoordSet()
  const cubeRocks = newCoordSet()

  let row = lines.length

  for (const line of lines) {
    findOccurences(line, ROCK.round).map(col => [row, col]).forEach(roundRocks.add)
    findOccurences(line, ROCK.cube).map(col => [row, col]).forEach(cubeRocks.add)

    row -= 1
  }
  return { roundRocks, cubeRocks }
}

function main () {
  const lines = []

  rl.on('line', line => {
    line = line.toString()

    lines.push(line)
  })

  rl.on('close', () => {
    console.log('partOne', partOne(index(lines)))
    console.log('partTwo', partTwo(index(lines)))
  })
}

main()

/** @param {{
 * roundRocks: CoordSet
 * cubeRocks: CoordSet
 * }} _
 * @returns {number}
 */
function partOne ({ roundRocks, cubeRocks }) {
  const rocks = [...roundRocks.values()].sort(([a], [b]) => b - a)
  for (const rock of rocks) {
    let [row, col] = rock
    while (row < rocks[0][0]) {
      if (roundRocks.has([row + 1, col]) || cubeRocks.has([row + 1, col])) {
        break
      }
      row += 1
    }
    roundRocks.delete(rock)
    roundRocks.add([row, col])
  }

  // too low: 102661
  return [...roundRocks.values()]
    .map(([row]) => row)
    .reduce(sum)
}

function partTwo (lines) {
  return 'todo'
}
