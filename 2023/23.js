const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

const { newCoordSet } = require('../utils.js')

/**
 * @typedef {import('../utils').Coord} Coord
 * @typedef {import('../utils').CoordSet} CoordSet
 */

/** @type {(a:Coord, b:Coord) => boolean} */
const eq = (a, b) => a.toString() === b.toString()

function main () {
  /** @type {string[][]} */
  const lines = []

  rl.on('line', line => {
    line = line.split('')

    lines.push(line)
  })

  rl.on('close', () => {
    const start = [0, lines[0].indexOf('.')]
    const end = [lines.length - 1, lines[lines.length - 1].indexOf('.')]
    console.log('partOne', partOne(lines, start, end))
    console.log('partTwo', partTwo(lines, start, end))
  })
}

main()

/** @type {(grid:string[][], start: Coord, end: Coord)} */
function partOne (grid, start, end) {
  const endString = end.toString()

  /** @type {(row:number,col:number, allowed:string) => [number,number]} */
  const C = (row, col, allowed) => {
    const char = grid[row] && grid[row][col]
    if (allowed.includes(char)) {
      return [row, col]
    }
  }
  /** @type {(_:Coord) => Coord[]} */
  function getNeighbours ([row, col]) {
    return [
      C(row, col + 1, '.>'),
      C(row, col - 1, '.<'),
      C(row + 1, col, '.v'),
      C(row - 1, col, '.^'),
    ].filter(Boolean)
  }

  const queue = []
  queue.push({ coord: start, prev: '', steps: 0 })

  let max = 0
  let visiting
  while ((visiting = queue.pop())) {
    if (visiting.coord.toString() === endString) {
      max = Math.max(max, visiting.steps)
      continue
    }
    for (const coord of getNeighbours(visiting.coord)) {
      if (coord.toString() === visiting.prev.toString()) {
        // do not step back
        continue
      }
      queue.push({
        coord,
        prev: visiting.coord,
        steps: visiting.steps + 1,
      })
    }
  }

  return max
}

function makeGetNeighbours (grid) {
  /** @type {(row:number,col:number) => Coord|undefined} */
  const C = (row, col) => {
    const char = grid[row] && grid[row][col]
    if (char && char !== '#') {
      return [row, col]
    }
  }

  /** @type {(_:Coord) => Coord[]} */
  const getNeighbours = ([row, col]) => [
    C(row, col + 1),
    C(row, col - 1),
    C(row + 1, col),
    C(row - 1, col),
  ].filter(Boolean)

  return getNeighbours
}

/**
 * Depth First Search that Did Not Finish on real input
 * @type {(grid:string[][], start: Coord, end: Coord)}
 */
function partTwoDfsDnf (grid, start, end) {
  const endString = end.toString()
  const getNeighbours = makeGetNeighbours(grid)

  /** @type {(visited: CoordSet, coord:Coord, steps:number) => number */
  function hike (visited, coord, steps) {
    const queue = [{ coord, steps }]

    let next
    while ((next = queue.pop())) {
      if (next.coord.toString() === endString) return next.steps
      visited.add(next.coord)

      const nbs = getNeighbours(next.coord).filter(c => !visited.has(c))
      if (nbs.length === 0) { // dead end
        return 0
      }
      if (nbs.length === 1) {
        queue.push({ coord: nbs[0], steps: next.steps + 1 })
        continue
      }

      let max = 0
      for (const nb of nbs) {
        const ans = hike(visited.clone(), nb, next.steps + 1)
        max = Math.max(max, ans)
      }
      return max
    }
  }

  return hike(newCoordSet(), start, 0)
}

function newEdgeMap () {
  const edgeMap = {
    /** @type {Map<string, [Coord, number][]>} */
    edges: new Map(),

    /** @type {(from:Coord, to:Coord, steps: number) => void} */
    add: (from, to, steps) => {
      const fs = from.toString()
      const em = edgeMap.edges
      if (!em.has(fs)) {
        em.set(fs, [])
      }
      em.get(fs).push([to, steps])
    },

    /** @param {Coord} from */
    get: (from) => edgeMap.edges.get(from.toString()),
  }
  return edgeMap
}

/**
 * @type {(grid:string[][], start: Coord, end: Coord)}
 */
function partTwo (grid, start, end) {
  const getNeigbours = makeGetNeighbours(grid)
  function findIntersections () {
    const intersections = []

    for (let row = 0; row < grid.length; row += 1) {
      for (let col = 0; col < grid[0].length; col += 1) {
        const char = grid[row] && grid[row][col]
        if (char === '#') continue
        const nbs = getNeigbours([row, col])
        if (nbs.length <= 2) continue
        intersections.push([row, col])
      }
    }

    return intersections
  }

  const vertices = newCoordSet()
  const edges = newEdgeMap()

  vertices.add(start)
  vertices.add(end)

  findIntersections().map(vertices.add)

  for (const from of vertices.values()) {
    const queue = getNeigbours(from)
    const visited = newCoordSet()
    visited.add(from)
    let n
    while ((n = queue.shift())) {
      let steps = 1

      while (true) {
        visited.add(n)
        const nbs = getNeigbours(n).filter(nb => !visited.has(nb))

        if (nbs.length !== 1) {
          throw new Error()
        }
        const to = nbs[0]
        if (vertices.has(to)) { // do not add vertices back
          edges.add(from, to, steps + 1)
          break
        }

        steps += 1
        n = to
      }
    }
  }

  const hiked = newCoordSet()

  /** @type {(trail:Coord, steps: number)} */
  function hike (trail, steps) {
    if (eq(trail, end)) return steps

    let max = 0
    hiked.add(trail)
    for (const [coord, n] of edges.get(trail)) {
      if (hiked.has(coord)) continue
      max = Math.max(max, hike(coord, n + steps))
    }
    hiked.delete(trail)

    return max
  }

  return hike(start, 0)
}
