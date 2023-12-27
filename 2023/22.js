const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

/**
 * @typedef {[number,number,number]} Coord
 *
 * @typedef {{
 *  id: number
 *  from:Coord
 *  to:Coord
 *  coords:Coords
 *  supports: number[]
 *  supportedBy: number[]
 * }} Brick
 */

/** @type {(from:Coord, to:Coord) => Coord} */
const direction = (from, to) =>
  R.zipWith((a, b) => a === b ? 0 : a < b ? 1 : -1, from, to)

/** @type {(coord:Coord, dir:Coord) => Coord} */
const move = (coord, dir) => R.zipWith(sum, coord, dir)

/** @type {(c1:Coord, c2:Coord) => boolean} */
const equals = (c1, c2) => c1.reduce(
  (acc, elem, id) => acc && elem === c2[id],
  true,
)

const fill = (from, to) => {
  const dir = direction(from, to)

  const coords = [from]
  let next = from
  while (true) {
    next = move(next, dir)
    coords.push(next)
    if (equals(next, to)) break
  }
  return coords
}

function main () {
  /** @type {Map<number, Brick>} */
  const bricks = new Map()

  let id = 0
  rl.on('line', line => {
    id += 1
    const [f, t] = line.split('~')

    const from = f.split(',').map(int)
    const to = t.split(',').map(int)
    bricks.set(id, {
      id,
      from,
      to,
      coords: fill(from, to),
      supports: [],
      supportedBy: [],
    })
  })

  rl.on('close', () => {
    console.log('partOne', partOne(bricks))
    console.log('partTwo', partTwo(bricks))
  })
}

main()

/** @type {(cs:Coord[]) => boolean} */
const onBottom = cs => cs.some(([,, z]) => z === 1)

/** @type {(cs:Coord[], c:Coord) => boolean} */
const includes = (cs, c) => cs.reduce((acc, curr) => acc || equals(c, curr), false)
/** @type {(cs1: Coord[], cs2: Coord[]) => boolean } */
const overlaps = (cs1, cs2) => cs1.some(c => includes(cs2, c))

const lowestFirst = (c1, c2) => c1.coords[0][2] - c2.coords[0][2]

/** @type {(cs:Coord[]) => Coord[]} */
const drop = cs => cs.map(coord => move(coord, [0, 0, -1]))

/** @type {(bricks: Map<number,Brick>) => number} */
function partOne (bricks) {
  /** @type {Brick[]} */
  const sorted = [...bricks.values()].sort(lowestFirst)
  const landed = []
  for (const brick of sorted) {
    while (true) {
      if (onBottom(brick.coords)) {
        landed.push(brick)
        break
      }
      const dropped = drop(brick.coords)
      const supporters = landed.filter(({ coords }) => overlaps(coords, dropped))

      if (supporters.length > 0) {
        supporters.forEach(s => {
          s.supports.push(brick.id)
          brick.supportedBy.push(s.id)
        })
        landed.push(brick)
        break
      }
      brick.coords = dropped
    }
  }

  for (const brick of bricks.values()) {
    brick.canDisintegrate = true
    if (brick.supports.length === 0) {
      continue
    }
    for (const id of brick.supports) {
      if (bricks.get(id).supportedBy.length === 1) {
        brick.canDisintegrate = false
      }
    }
  }

  return [...bricks.values()].filter(b => b.canDisintegrate).length
}

function partTwo (lines) {
  return 'todo'
}
