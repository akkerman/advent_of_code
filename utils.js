/**
 * @typedef {[number, number]} Coord
 */

/**
 * Split a list in sub lists of given length
 * @template T
 * @param {number} n
 * @param {Array<T>} xs
 * @returns {Array<Array<T>>}
 */
const splitList = (n, xs) => (xs.length)
  ? [xs.slice(0, n)].concat(splitList(n, xs.slice(n)))
  : xs

/**
 * Find elements that all given arrays have in common
 * @template T
 * @param {Array<Array<T>>} arrays
 * @returns {Array<T>}
 */
const intersection = arrays => [...new Set(arrays[0].filter(
  item => arrays.slice(1).reduce(
    (acc, curr) => acc && curr.includes(item),
    true,
  ),
))]

/**
 * Greatest common divisor
 * Grootste gemene deler
 *
 * on array: numbers.reduce(gcd)
 *
 * @type {(x:number,y:number) => number}
 */
function gcd (x, y) {
  while (y) {
    const t = y
    y = x % y
    x = t
  }
  return x
}

/**
 * Least common multiple
 * Kleinste gemene veelvoud
 *
 * on array: numbers.reduce(lcm)
 *
 * @type {(x:number,y:number) => number}
 */
function lcm (x, y) {
  return Math.abs(x * (y / gcd(x, y)))
}

/**
 * Find occurences of str in line
 * @param {string} line
 * @param {string} str
 * @returns {number[]} indices
 */
function findOccurences (line, str) {
  const rgx = new RegExp(str, 'gi')
  const indices = []
  let result = {}
  while ((result = rgx.exec(line))) {
    indices.push(result.index)
  }
  return indices
}

/**
 * Calculate manhattan distance of two coordinates.
 * @param {Coord} coord1
 * @param {Coord} coord2
 * @returns {number}
 */
const manhattanDistance = ([x1, y1], [x2, y2]) =>
  Math.abs(x1 - x2) + Math.abs(y1 - y2)

/** @typedef {any[][]} Matrix */

/**
 * Rotate matrix counter clock wise
 * @type {(matrix: Matrix) => Matrix}
 */
const rotateL = matrix => matrix[0].map((_, index) => matrix.map(row => row[row.length - 1 - index]))
/**
 * Rotate matrix clock wise
 * @type {(matrix: Matrix) => Matrix}
 */
const rotateR = matrix => matrix[0].map((_, index) => matrix.map(row => row[index]).reverse())

/**
 * Flip matrix over diagonal, from left upper to right lower
 * @type {(matrix: Matrix) => Matrix}
 */
const flip = matrix => matrix[0].map((_, index) => matrix.map(row => row[index]))

/** @typedef {ReturnType<newCoordSet>} CoordSet */

/** @param {Map<string, Coord>} coords */
const newCoordSet = (coords) => {
  const coordSet = {
  /** @type {Map<string, Coord>} */
    coords: coords || new Map(),
    /** @type {(coord: Coord) => boolean} */
    has: coord => coordSet.coords.has(coord.toString()),
    /** @type {(coord: Coord) => void} */
    add: coord => coordSet.coords.set(coord.toString(), coord),
    /** @type {(coord: Coord) => boolean} */
    delete: coord => coordSet.coords.delete(coord.toString()),

    values: () => coordSet.coords.values(),

    clone: () => newCoordSet(new Map(coordSet.coords)),
  }
  return coordSet
}

module.exports = {
  newCoordSet,
  splitList,
  intersection,
  lcm,
  gcd,
  findOccurences,
  manhattanDistance,
  rotateL,
  rotateR,
  flip,
}
