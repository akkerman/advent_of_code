const readline = require('readline')
const { newCoordSet, findOccurences } = require('../utils.js')
const rl = readline.createInterface({ input: process.stdin })
const sum = (a,b) => a+b // eslint-disable-line

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
    console.log('partTwo', partTwo({ lines, ...index(lines) }))
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

/** @param {{
 * lines: string[]
 * roundRocks: CoordSet
 * cubeRocks: CoordSet
 * }} _
 * @returns {number}
 */
function partTwo ({ lines, roundRocks, cubeRocks }) {
  const maxRow = lines.length
  const maxCol = lines[0].length - 1
  const cache = new Map()

  const toString = coords => JSON.stringify(
    coords.sort(([a1, a2], [b1, b2]) => (100 * a1 + a2) - (100 * b1 + b2)),
  )

  function cycle (coords) {
    const coordsStr = toString(coords)
    if (cache.has(coordsStr)) {
      return [cache.get(coordsStr), true]
    }
    roundRocks = newCoordSet()
    coords.forEach(roundRocks.add)

    // NORTH ====================
    let rocks = [...roundRocks.values()].sort(([a], [b]) => b - a)

    for (const rock of rocks) {
      let [row, col] = rock
      while (row < maxRow) {
        if (roundRocks.has([row + 1, col]) || cubeRocks.has([row + 1, col])) {
          break
        }
        row += 1
      }
      roundRocks.delete(rock)
      roundRocks.add([row, col])
    }

    // WEST ====================
    rocks = [...roundRocks.values()].sort(([, a], [, b]) => a - b)

    for (const rock of rocks) {
      let [row, col] = rock
      while (col > 0) {
        if (roundRocks.has([row, col - 1]) || cubeRocks.has([row, col - 1])) {
          break
        }
        col -= 1
      }
      roundRocks.delete(rock)
      roundRocks.add([row, col])
    }

    // SOUTH ====================
    rocks = [...roundRocks.values()].sort(([a], [b]) => a - b)

    for (const rock of rocks) {
      let [row, col] = rock
      while (row > 1) {
        if (roundRocks.has([row - 1, col]) || cubeRocks.has([row - 1, col])) {
          break
        }
        row -= 1
      }
      roundRocks.delete(rock)
      roundRocks.add([row, col])
    }

    // EAST ====================
    rocks = [...roundRocks.values()].sort(([, a], [, b]) => b - a)

    for (const rock of rocks) {
      let [row, col] = rock
      while (col < maxCol) {
        if (roundRocks.has([row, col + 1]) || cubeRocks.has([row, col + 1])) {
          break
        }
        col += 1
      }
      roundRocks.delete(rock)
      roundRocks.add([row, col])
    }

    coords = [...roundRocks.coords.values()]

    cache.set(coordsStr, coords)

    return [coords, false]
  }

  let coords = [...roundRocks.coords.values()]

  const cycles = 1000000000
  const iterations = new Map()
  for (let i = 1; i <= cycles; i += 1) {
    const [c, repeat] = cycle(coords)
    coords = c
    let firstIteraton = 0
    if (repeat) {
      if (firstIteraton === 0) { firstIteraton = i }
      const s = toString(coords)
      if (iterations.has(s)) {
        const idx = (cycles - firstIteraton) % iterations.size

        const coords = [...iterations.keys()][idx]
        return JSON.parse(coords)
          .map(([row]) => row)
          .reduce(sum)
      }
      iterations.set(s, i)
    }
  }

  return coords
    .map(([row]) => row)
    .reduce(sum)
}
