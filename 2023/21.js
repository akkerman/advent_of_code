const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

const { findOccurences } = require('../utils')

/**
 * @typedef {import('../utils').Coord} Coord
 */

function main () {
  /** @type {string[]} */
  const garden = []
  /** @type {Coord} */
  let start

  let row = 0
  rl.on('line', line => {
    const col = line.indexOf('S')
    if (col !== -1) {
      start = [row, col]
    }
    garden.push(line)
    row += 1
  })

  rl.on('close', () => {
    console.log('partOne', partOne(garden, start))
    console.log('partTwo', partTwo(garden, start))
  })
}

main()

function partOne (garden, start) {
  function makeCoordMap () {
    const p = {
      /** @type {Map<string,Coord>} */
      coords: new Map(),
      /** @type {(coord:Coord)=>void} */
      add: coord => p.coords.set(coord.toString(), coord),
      /** @type {(coord:Coord)=>boolean} */
      has: coord => p.coords.has(coord.toString()),
    }
    return p
  }

  const rocks = makeCoordMap()

  for (let row = 0; row < garden.length; row += 1) {
    findOccurences(garden[row], '#')
      .forEach(col => rocks.add([row, col]))
  }

  /** @type {(coord:Coord) => Coord[]} */
  const getNeigbours = ([row, col]) => [
    [row - 1, col],
    [row + 1, col],
    [row, col - 1],
    [row, col + 1],
  ].filter(c => !rocks.has(c))

  const limit = (garden.length < 20) ? 6 : 64

  let current = makeCoordMap()
  let next = makeCoordMap()
  current.add(start)
  for (let iteration = 1; iteration <= limit; iteration += 1) {
    for (const coord of current.coords.values()) {
      getNeigbours(coord).forEach(next.add)
    }
    current = next
    next = makeCoordMap()
  }

  return current.coords.size
}

function partTwo (garden) {
  return 'todo'
}
