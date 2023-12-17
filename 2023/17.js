const Heap = require('heap')
const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

/**
 * @typedef {number} HeatLoss
 * @typedef {HeatLoss[][]} CityMap
 * @typedef {import('../utils').Coord} Coord
 * @typedef {{
 *   coord: Coord
 *   direction: Coord
 *   steps: number
 *   loss: number
 * }} Movement
 */

function main () {
  const lines = []

  rl.on('line', line => {
    line = line.split('').map(int)

    lines.push(line)
  })

  rl.on('close', () => {
    console.log('partOne', partOne(lines))
    console.log('partTwo', partTwo(lines))
  })
}

main()

const UP = [-1, 0]
const DOWN = [1, 0]
const LEFT = [0, -1]
const RIGHT = [0, 1]
const S_UP = UP.toString()
const S_DOWN = DOWN.toString()
const S_LEFT = LEFT.toString()
const S_RIGHT = RIGHT.toString()

/** @type {(cityMap:CityMap, startMovement: Movement, endCoord: Coord) => number} */
function solve (cityMap, startMovement, endCoord) {
  /** @type {(movement:Movement, newDirection:coord) => Movement|undefined} */
  const step = ({ coord: [x, y], direction, steps }, newDirection) => {
    const sameDir = direction.toString() === newDirection.toString()
    const [dx, dy] = newDirection

    return {
      coord: [x + dx, y + dy],
      direction: [dx, dy],
      steps: sameDir ? steps + 1 : 1,
    }
  }

  /** @type {(move:Movement) => Movement[]} */
  function getAllNeighbours (move) {
    const dir = move.direction.toString()
    return [
      dir !== S_UP && step(move, DOWN),
      dir !== S_RIGHT && step(move, LEFT),
      dir !== S_DOWN && step(move, UP),
      dir !== S_LEFT && step(move, RIGHT),
    ].filter(Boolean)
  }

  /** @type {(coord:Coord) => number|undefined} */
  const getHeatLoss = ([x, y]) => cityMap[x] && cityMap[x][y]

  /** @type {(move:Movement) => Movement[]} */
  const getNeighbours = move => getAllNeighbours(move).filter(
    nb => nb.steps <= 3 && getHeatLoss(nb.coord) !== undefined,
  )

  const endCoordStr = endCoord.toString()

  const prio = new Heap((a, b) => a.heatLoss - b.heatLoss)

  prio.push(startMovement)

  const visited = new Set()

  while (true) {
    const movement = prio.pop()
    const { heatLoss, ...mv } = movement
    const str = JSON.stringify(mv)
    if (visited.has(str)) continue
    visited.add(str)

    const nbs = getNeighbours(mv)

    for (const nb of nbs) {
      const currentHeatloss = getHeatLoss(nb.coord)
      if (nb.coord.toString() === endCoordStr) return heatLoss + currentHeatloss
      prio.push({ ...nb, heatLoss: heatLoss + currentHeatloss })
    }
  }
}
/** @type {(map:CityMap) => number} */
function partOne (lines) {
  const startMovement = { coord: [0, 0], direction: [0, 1], steps: 0, heatLoss: 0 }
  const endCoord = [lines.length - 1, lines[0].length - 1]
  return solve(lines, startMovement, endCoord)
}

/** @type {(map:CityMap) => number} */
function partTwo (lines) {
  return 'todo'
}
