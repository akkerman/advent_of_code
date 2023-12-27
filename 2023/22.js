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
  /** @type {Brick[]} */
  const bricks = []

  rl.on('line', line => {
    const [f, t] = line.split('~')

    const from = f.split(',').map(int)
    const to = t.split(',').map(int)
    bricks.push({
      from,
      to,
      coords: fill(from, to),
      supports: [],
      supportedBy: [],
    })
  })

  rl.on('close', () => {
    solve(bricks)
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
function solve (bricks) {
  /** @type {Brick[]} */
  const sorted = bricks.sort(lowestFirst).map((b, id) => ({ ...b, id }))
  sorted.forEach((b, i) => { b.id = i })
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

  for (const brick of sorted) {
    brick.canDisintegrate = true
    if (brick.supports.length === 0) {
      continue
    }
    for (const id of brick.supports) {
      if (sorted[id].supportedBy.length === 1) {
        brick.canDisintegrate = false
      }
    }
  }

  log('partOne', sorted.filter(b => b.canDisintegrate).length)

  for (let i = 0; i < sorted.length; i += 1) {
    const fallen = new Set()
    fallen.add(sorted[i].id)
    for (let j = i + 1; j < sorted.length; j += 1) {
      const supportersHaveFallen = sorted[j].supportedBy.length > 0 &&
        sorted[j].supportedBy.reduce((acc, curr) => acc && fallen.has(curr), true)
      if (supportersHaveFallen) {
        fallen.add(sorted[j].id)
      }
    }
    sorted[i].cascade = fallen.size - 1 // count only other bricks
  }

  log('partTwo', sorted.map(b => b.cascade)
    .reduce(sum))
}
