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

    partTwoAnalysis(garden, start)
  })
}

main()

function solver (garden, start, limit) {
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

  return {
    rocks: rocks.coords,
    reachable: current.coords,
  }
}

function partOne (garden, start) {
  const limit = (garden.length < 20) ? 6 : 64
  return solver(garden, start, limit).reachable.size
}

function partTwoAnalysis (garden, start) {
/**
 * @param {Set<string>} rocks
 * @param {Set<string>} reachable
 * @param {number} width
 */
  const draw = (rocks, reachable, width) => {
    const garden = Array.from(
      { length: width },
      () => Array.from(
        { length: width },
        () => '.'))

    const plot = (positions, char) => {
      for (const [x, y] of positions.values()) {
        garden[x][y] = char
      }
    }

    plot(reachable, 'O')
    plot(rocks, '#')

    for (const row of garden) {
      log(row.join(''))
    }
  }

  garden = garden.map(line => line + line + line + line + line)

  const m = garden.length
  for (let r = 1; r < 5; r += 1) {
    for (let i = 0; i < m; i += 1) {
      garden.push(garden[i])
    }
  }

  const [r, c] = start

  const { reachable, rocks } = solver(garden, [r + 2 * m, c + 2 * m], r + 2 * m)

  draw(rocks, reachable, garden.length)
  return log('rechable on 5x5 garden map', reachable.size)
}

function partTwo (garden) {
  return 'todo'
}
