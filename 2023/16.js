const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

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

function print (beam) {
  const [x, y] = beam.coord
  log({
    ...beam,
    coord: [x + 1, y + 1],
  })
}

const UP = [-1, 0]
const DOWN = [1, 0]
const LEFT = [0, -1]
const RIGHT = [0, 1]
const S_UP = UP.toString()
const S_DOWN = DOWN.toString()
const S_LEFT = LEFT.toString()
const S_RIGHT = RIGHT.toString()

/** @type {string[][]} */
function partOne (contraption) {
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
    //  . = direction constant
    //  / = switch up&right, down&left
    //  \ = switch up&left, down&right
    //  | = up and down when left or right
    //  - = left and right when up or down

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
  const queue = [{ coord: [0, 0], direction: RIGHT }]
  energized.add('0,0')

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
  return 'todo'
}
