const { intersection } = require('ramda')
require('util').inspect.defaultOptions.depth = null
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

/**
 * @typedef {Array<Number>} Coordinate
 * @typedef {Array<Coordinate>} Pair
 * @typedef {Array<Coordinate>} CoordinateList
 * @typedef {Number} Distance
 *
 * @typedef Scanner
 * @type Object
 * @property {Number} id
 * @property {Boolean} processed
 * @property {CoordinateList} beacons
 * @property {Map<Distance, Pair>} distances
 * @property {Array<CoordinateList>} rotations
 */

const scanners = new Map()
let id

let scannerPattern = 'nix'

scannerPattern = /.+scanner (\d+) .+/

rl.on('line', data => {
  if (data === '') return
  const matches = data.match(scannerPattern)
  if (matches) {
    id = matches[1]
    scanners.set(id, { id, beacons: [] })
    return
  }
  scanners.get(id).beacons.push(data.split(',').map(Number))
})

rl.on('close', () => {
  prepScanners()
  console.log('partOne', partOne()) // 383
  console.log('partTwo', partTwo())
})

/**
 * @param {Coordinate} a
 * @param {Coordinate} b
 * @returns {Distance}
 */
const euclideanDistance = (a, b) =>
  Math.hypot(...Object.keys(a).map(k => b[k] - a[k]))

const possibleRotations = [
  ([x, y, z]) => [x, y, z],
  ([x, y, z]) => [x, -y, -z],
  ([x, y, z]) => [x, z, -y],
  ([x, y, z]) => [x, -z, y],

  ([x, y, z]) => [y, z, x],
  ([x, y, z]) => [y, -z, -x],
  ([x, y, z]) => [y, x, -z],
  ([x, y, z]) => [y, -x, z],

  ([x, y, z]) => [z, x, y],
  ([x, y, z]) => [z, -x, -y],
  ([x, y, z]) => [z, y, -x],
  ([x, y, z]) => [z, -y, x],

  ([x, y, z]) => [-x, z, y],
  ([x, y, z]) => [-x, -z, -y],
  ([x, y, z]) => [-x, y, -z],
  ([x, y, z]) => [-x, -y, z],

  ([x, y, z]) => [-y, x, z],
  ([x, y, z]) => [-y, -x, -z],
  ([x, y, z]) => [-y, z, -x],
  ([x, y, z]) => [-y, -z, x],

  ([x, y, z]) => [-z, y, x],
  ([x, y, z]) => [-z, -y, -x],
  ([x, y, z]) => [-z, x, -y],
  ([x, y, z]) => [-z, -x, y],
]

/**
 * @param {CoordinateList} coords
 * @returns {Array<CoordinateList>} rotations
 */
function rotate (coords) {
  const rotations = Array.from({ length: 24 }, () => [])
  for (const coord of coords) {
    for (let i = 0; i < 24; i += 1) {
      rotations[i].push(possibleRotations[i](coord))
    }
  }
  return rotations
}

/**
 * Generate pairs.
 * do not pair a coordinate with itself
 * do not pair coordinates twice i.e.
 * treat it as if [a, b] === [b, a]
 * @param {CoordinateList} coords
 * @generator
 * @yields {Pair}
 */
function * generatePairs (coords) {
  const max = coords.length
  for (let i = 0; i < max; i += 1) {
    for (let j = i + 1; j < max; j += 1) {
      yield [coords[i], coords[j]]
    }
  }
}

function distances (beacons) {
  const dist = new Map()
  for (const [from, to] of generatePairs(beacons)) {
    dist.set(
      euclideanDistance(from, to),
      [from, to],
    )
  }
  return dist
}

function prepScanners () {
  for (const scanner of scanners.values()) {
    scanner.distances = distances(scanner.beacons)
    scanner.rotations = rotate(scanner.beacons)
  }
}

/**
 * @param {Map<Distance, Pair>} theUniverse
 * @param {Scanner[]} scanners
 * @return {[Scanner,Distance]}
 */
function findMatchingDistance (theUniverse, scanners) {
  const universeDistances = [...theUniverse.keys()]
  for (const scanner of scanners) {
    if (scanner.processed) continue
    const overlap = intersection(universeDistances, [...scanner.distances.keys()])
    if (overlap.length >= 12) {
      return [scanner, overlap[0]]
    }
  }
  return []
}

/**
 * @param {Pair} U beacon pair from normalized universe
 * @param {Pair} S beacon pair from scanner
 * @return {{
 *   match1: Pair
 *   match2: Pair
 *   base: Coordinate
 * }}
 */
const matchPair = (U, S) => {
  let d1 = euclideanDistance(U[0], S[1])
  let d2 = euclideanDistance(U[1], S[0])
  if (d1 === d2) {
    return {
      match1: [U[0], S[1]],
      match2: [U[1], S[0]],

      base: [U[0][0] - S[1][0], U[0][1] - S[1][1], U[0][2] - S[1][2]],
    }
  }

  d1 = euclideanDistance(U[0], S[0])
  d2 = euclideanDistance(U[1], S[1])
  if (d1 === d2) {
    return {
      match1: [U[0], S[0]],
      match2: [U[1], S[1]],

      base: [U[0][0] - S[0][0], U[0][1] - S[0][1], U[0][2] - S[0][2]],
    }
  }
}

/**
 * @param {Map<Distance, Pair>} theUniverse
 * @param {Scanner[]} scanners
 * @return {Map<Distance, Pair>} - sub universe
 */
function normalize (theUniverse, scanner, distance) {
  const universePair = theUniverse.get(distance)
  const [scannerBeacon1, scannerBeacon2] = scanner.distances.get(distance)
  const idx1 = scanner.beacons.findIndex(b => b === scannerBeacon1)
  const idx2 = scanner.beacons.findIndex(b => b === scannerBeacon2)

  for (let rot = 0; rot < 24; rot += 1) {
    const scannerPair = [
      scanner.rotations[rot][idx1],
      scanner.rotations[rot][idx2],
    ]
    const result = matchPair(universePair, scannerPair)
    if (result) {
      const [x, y, z] = result.base

      const normalizedBeacons = scanner.rotations[rot].map(([x1, y1, z1]) => [x + x1, y + y1, z + z1])

      return distances(normalizedBeacons)
    }
  }
}

function partOne () {
  /** @type Scanner */
  const scanner0 = scanners.get('0')
  let theUniverse = new Map(scanner0.distances)
  scanner0.processed = true

  while (true) {
    const [scanner, distance] = findMatchingDistance(theUniverse, scanners.values())
    const subUniverse = normalize(theUniverse, scanner, distance)

    theUniverse = new Map([...theUniverse, ...subUniverse])

    scanner.processed = true
    if ([...scanners.values()].every(s => s.processed)) break
  }

  const beacons = new Set()
  for (const [b1, b2] of theUniverse.values()) {
    beacons.add(JSON.stringify(b1))
    beacons.add(JSON.stringify(b2))
  }
  return beacons.size
}

function partTwo () {
  return 'todo'
}
