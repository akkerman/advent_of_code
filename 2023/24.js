const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

function main () {
  const storms = []

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
    storms.push({
      id,
      pos,
      vel,
      crosses: [],
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
    const area = storms.length === 5
      ? [7, 27]
      : [200000000000000, 400000000000000]

    const minmax = { maxX, minX, maxY, minY, maxZ, minZ }
    const length = [maxX + minX, maxY + minY, maxZ + minZ]
    console.log('partOne', partOne(storms, area, length))
    console.log('partTwo', partTwo(storms, area, length))
  })
}

main()

/**
 * @type {[Coord,Coord]} line1
 * @type {[Coord,Coord]} line2
 */
function intersect (line1, line2) {
  // https://stackoverflow.com/a/20677983/3889449
  const xdiff = [line1[0][0] - line1[1][0], line2[0][0] - line2[1][0]]
  const ydiff = [line1[0][1] - line1[1][1], line2[0][1] - line2[1][1]]

  // GVD!!
  const det = (a, b) => parseInt(BigInt(a[0]) * BigInt(b[1]) - BigInt(a[1]) * BigInt(b[0]))

  const div = det(xdiff, ydiff)
  if (div === 0) { return null }

  const d = [det(line1[0], line1[1]), det(line2[0], line2[1])]
  const x = det(d, xdiff) / div
  const y = det(d, ydiff) / div
  return [x, y]
}

/** @type {(coord:Coord, dir:Coord) => Coord} */
const add = (coord, dir) => R.zipWith(sum, coord, dir)

const sub = (coord, dir) => R.zipWith((a, b) => a - b, coord, dir)

const direction = (from, to) => {
  const retval = R.zipWith((a, b) => b - a, from, to)
  return retval
}

const sameDirection = ([x1, y1], [x2, y2]) => {
  const retval = Math.sign(x1) === Math.sign(x2) &&
  Math.sign(y1) === Math.sign(y2)
  return retval
}

function partOne (storms, [min, max], lengths) {
  let sum = 0
  for (let i = 0; i < storms.length; i += 1) {
    for (let j = i + 1; j < storms.length; j += 1) {
      if (i === j) continue // same storm
      const { id: id1, pos: p1, vel: v1 } = storms[i]
      const { id: id2, pos: p2, vel: v2 } = storms[j]
      const cross = intersect([p1, add(p1, v1)], [p2, add(p2, v2)])

      if (cross) {
        const [x, y] = cross
        if (min <= x && x <= max &&
           min <= y && y <= max) {
          // log('in test area')
          const crossI = { id2, cross, future: false }
          const crossJ = { id1, cross, future: false }
          if (sameDirection(direction(p1, cross), v1) &&
              sameDirection(direction(p2, cross), v2)) {
            // log('in future')
            crossI.future = true
            crossJ.future = true

            sum += 1
          }
          storms[i].crosses.push(crossI)
        }
      }
    }
  }

  // storms.slice(0, 1).forEach(({ crosses, ...rest }) => log('storm', rest, 'crosses', crosses))

  // too low 21662
  return sum
}

function partTwo (storms, { min, max }) {
  return 'todo'
}
