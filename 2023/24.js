const { init } = require('z3-solver')
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
 * @typedef {{
 * id:number
 * pos: Coord
 * vel: Coord
 * }} Stone
 */

function main () {
  /** @type {Stone[]} */
  const stones = []

  let maxX = Number.MIN_SAFE_INTEGER
  let minX = Number.MAX_SAFE_INTEGER
  let maxY = Number.MIN_SAFE_INTEGER
  let minY = Number.MAX_SAFE_INTEGER
  let maxZ = Number.MIN_SAFE_INTEGER
  let minZ = Number.MAX_SAFE_INTEGER

  let id = 0
  rl.on('line', line => {
    id += 1
    const [p, v] = line.split(' @ ')

    const pos = p.split(', ').map(int)
    const vel = v.split(', ').map(int)
    stones.push({
      id,
      pos,
      vel,
    })

    const [x, y, z] = pos

    maxX = Math.max(x, maxX)
    minX = Math.min(x, minX)
    maxY = Math.max(y, maxY)
    minY = Math.min(y, minY)
    maxZ = Math.max(z, maxZ)
    minZ = Math.min(z, minZ)
  })

  rl.on('close', () => {
    const area = stones.length === 5
      ? [7, 27]
      : [200000000000000, 400000000000000]

    console.log('partOne', partOne(stones, area))
    console.log('partTwo', partTwo(stones, area))
  })
}

main()

/** @type {number[][] => bigint[][]} */
const toBigInt = line => line.map(coord => coord.map(BigInt))

/**
 * @param {[Coord,Coord]} line1
 * @param {[Coord,Coord]} line2
 * @returns {[number,number]|undefined}
 */
function intersect (line1, line2) {
  line1 = toBigInt(line1)
  line2 = toBigInt(line2)
  // https://stackoverflow.com/a/20677983/3889449
  const xdiff = [line1[0][0] - line1[1][0], line2[0][0] - line2[1][0]]
  const ydiff = [line1[0][1] - line1[1][1], line2[0][1] - line2[1][1]]

  const det = (a, b) => a[0] * b[1] - a[1] * b[0]

  const div = det(xdiff, ydiff)
  if (div === BigInt(0)) {
    return null // doesn't intersect
  }

  const d = [det(line1[0], line1[1]), det(line2[0], line2[1])]
  const x = det(d, xdiff) / div
  const y = det(d, ydiff) / div
  return [parseInt(x), parseInt(y)]
}

/** @type {(coord:Coord, dir:Coord) => Coord} */
const add = (coord, dir) => R.zipWith(sum, coord, dir)

/** @type {(from:Coord, to:Coord) => Coord} */
const direction = (from, to) => R.zipWith((a, b) => b - a, from, to)

/** @type {(from:Coord, to:Coord) => boolean} */
const sameDirection = ([x1, y1], [x2, y2]) =>
  Math.sign(x1) === Math.sign(x2) &&
  Math.sign(y1) === Math.sign(y2)

/** @type {(stones:Stone[], [min,max]:[number,number]) => number} */
function partOne (stones, [min, max]) {
  let sum = 0
  for (let i = 0; i < stones.length; i += 1) {
    for (let j = i + 1; j < stones.length; j += 1) {
      if (i === j) continue // same stone
      const { pos: p1, vel: v1 } = stones[i]
      const { pos: p2, vel: v2 } = stones[j]
      const cross = intersect([p1, add(p1, v1)], [p2, add(p2, v2)])

      if (cross) {
        const [x, y] = cross
        if (min <= x && x <= max &&
           min <= y && y <= max) {
          // log('in test area')
          if (sameDirection(direction(p1, cross), v1) &&
              sameDirection(direction(p2, cross), v2)) {
            // log('in future')
            sum += 1
          }
        }
      }
    }
  }

  return sum
}

async function solve (stones) {
  const { Context, em } = await init()
  const { Solver, Int } = new Context('main')
  const solver = new Solver()
  const rockPos = [Int.const('x'), Int.const('y'), Int.const('z')]
  const rockVel = [Int.const('dx'), Int.const('dy'), Int.const('dz')]

  for (const { id, pos, vel } of stones) {
    const idn = Int.const('id' + id)

    for (let i = 0; i <= 2; i += 1) {
      solver.add(
        Int.val(pos[i])
          .add(idn.mul(vel[i]))
          .sub(rockPos[i])
          .sub(idn.mul(rockVel[i]))
          .eq(0),
      )
    }
  }

  await solver.check()
  em.PThread.terminateAllThreads()
  const answer = rockPos.map(c => solver.model().eval(c)).map(Number).reduce(sum)

  log(answer)
}

/** @type {(stones:Stone[], [min,max]:[number,number]) => ''} */
function partTwo (stones, [min, max]) {
  solve(R.take(3, stones))
  return ''
}
