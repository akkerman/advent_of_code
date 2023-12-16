const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

/**
 * @typedef {import('../utils').Coord} Coord
 * @typedef {{
 *   coord: Coord
 *   direction: Coord
 * }} BeamHead
 */

function main () {
  /** @type {string[][]} */
  const lines = []

  rl.on('line', line => {
    line = line.split('')

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

/** @type {string[][]} */
function partOne (contraption, start) {
  if (!start) {
    start = { coord: [0, 0], direction: RIGHT }
  }
  /** @type {(beam:BeamHead) => string} */
  function getTile (beam) {
    const [x, y] = beam.coord

    return contraption[x] && contraption[x][y]
  }

  /** @type {(beam:BeamHead, direction:Coord) => Coord} */
  const move = ({ coord: [x, y] }, [dx, dy]) => ({
    coord: [x + dx, y + dy],
    direction: [dx, dy],
  })

  /** @type {(beam:BeamHead) => BeamHead[]} */
  function getNeighbours (beam) {
    const dir = beam.direction.toString()
    const tile = getTile(beam)
    switch (tile) {
      case '.': return [move(beam, beam.direction)]
      case '/':
        switch (dir) {
          case S_UP: return [move(beam, RIGHT)]
          case S_DOWN: return [move(beam, LEFT)]
          case S_RIGHT: return [move(beam, UP)]
          case S_LEFT: return [move(beam, DOWN)]
          default: throw new Error(`On ${tile}, unknown direction ${dir}`)
        }
      case '\\':
        switch (dir) {
          case S_UP: return [move(beam, LEFT)]
          case S_DOWN: return [move(beam, RIGHT)]
          case S_RIGHT: return [move(beam, DOWN)]
          case S_LEFT: return [move(beam, UP)]
          default: throw new Error(`On ${tile}, unknown direction ${dir}`)
        }
      case '|':
        switch (dir) {
          case S_UP:
          case S_DOWN: return [move(beam, beam.direction)]
          case S_RIGHT:
          case S_LEFT: return [move(beam, UP), move(beam, DOWN)]
          default: throw new Error(`On ${tile}, unknown direction ${dir}`)
        }
      case '-':
        switch (dir) {
          case S_UP:
          case S_DOWN: return [move(beam, LEFT), move(beam, RIGHT)]
          case S_RIGHT:
          case S_LEFT: return [move(beam, beam.direction)]
          default: throw new Error(`On ${tile}, unknown direction ${dir}`)
        }

      default: throw new Error(`Unkown tile ${tile}`)
    }
  }

  const energized = new Set()
  const visited = new Set()
  const queue = [start]
  energized.add(start.coord.join(','))

  while (queue.length > 0) {
    const beam = queue.shift()
    const neighbours = getNeighbours(beam)
    for (const neighbour of neighbours) {
      if (!getTile(neighbour)) continue
      const nbs = JSON.stringify(neighbour)
      if (visited.has(nbs)) continue

      queue.push(neighbour)
      energized.add(neighbour.coord.join(','))
      visited.add(nbs)
    }
  }

  return energized.size
}

function partTwo (lines) {
  const startingPoints = []

  for (let col = 0; col < lines[0].length; col += 1) {
    startingPoints.push({ coord: [0, col], direction: DOWN })
    startingPoints.push({ coord: [lines.length - 1, col], direction: UP })
  }
  for (let row = 0; row < lines[0].length; row += 1) {
    startingPoints.push({ coord: [row, 0], direction: RIGHT })
    startingPoints.push({ coord: [row, lines[0].length - 1], direction: RIGHT })
  }

  return startingPoints.reduce(
    (maxEnergized, start) => Math.max(maxEnergized, partOne(lines, start)),
    0,
  )
}
