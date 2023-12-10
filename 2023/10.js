const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

/**
 * @typedef {[number,number]} Coord
 * @typedef {string[][]} Graph
 * @typedef {{
 *   coord: Coord
 *   cost: number
 * }} Node
 */

/** @type {() => void} */
function main () {
  const graph = []

  let r = 0
  let start

  rl.on('line', line => {
    line = line.split('')
    graph.push(line)

    const c = line.indexOf('S')
    if (c >= 0) {
      start = [r, c]

      if (r > 8) {
        line[c] = 'L' // input
      } else {
        line[c] = 'F' // example
      }
    }

    r += 1
  })

  rl.on('close', () => {
    const [distance, visited] = search(graph, start)
    console.log('partOne', distance)
    console.log('partTwo', partTwo(graph, visited))
  })
}

main()

/** @type {(graph:Graph) => (node:Node) => boolean} */
const makeIsInside = graph => ([r, c]) => !!(graph[r] && graph[r][c])

/**
 * @param {Graph} graph
 * @returns {(node:Node) => Node[]}
 */
function makeGetNeighbours (graph) {
  const isInside = makeIsInside(graph)
  return function getNeighbours (node) {
    let neighbours
    const [r, c] = node.coord
    const pipe = graph[r][c]

    switch (pipe) {
      case '|': // is a vertical pipe connecting north and south.
        neighbours = [[r - 1, c], [r + 1, c]]
        break
      case '-': // is a horizontal pipe connecting east and west.
        neighbours = [[r, c - 1], [r, c + 1]]
        break
      case 'L': // is a 90-degree bend connecting north and east.
        neighbours = [[r - 1, c], [r, c + 1]]
        break
      case 'J': // is a 90-degree bend connecting north and west.
        neighbours = [[r - 1, c], [r, c - 1]]
        break
      case '7': // is a 90-degree bend connecting south and west.
        neighbours = [[r, c - 1], [r + 1, c]]
        break
      case 'F': // is a 90-degree bend connecting south and east.
        neighbours = [[r, c + 1], [r + 1, c]]
        break
      default:
      // '.' of 'S'
        neighbours = []
    }

    return neighbours.filter(isInside)
      .map(coord => ({ coord, cost: 1 }))
  }
}

/** @type {(graph:Graph, start:Coord) => [number, Set<string>]} */
function search (graph, start) {
  const getNeighbours = makeGetNeighbours(graph)
  /** @type {Node[]} */
  const queue = [{ coord: start, cost: 0 }]
  const visited = {
    /** @type {Set<string>} */
    nodes: new Set(),
    /** @type {(node: Node) => boolean} */
    has: node => visited.nodes.has(node.coord.toString()),
    /** @type {(node: Node) => void} */
    add: node => visited.nodes.add(node.coord.toString()),
  }

  let cost = 0

  while (true) {
    const node = queue.shift()

    if (!node) break

    const neighbours = getNeighbours(node)
    for (const neighbour of neighbours) {
      if (visited.has(neighbour)) continue
      const updated = {
        ...neighbour,
        cost: node.cost + neighbour.cost,
      }
      queue.push(updated)
      visited.add(updated)

      cost = Math.max(cost, updated.cost)
    }
  }
  return [cost, visited.nodes]
}

/** @type {(graph:Graph, pipeCoords:Set<string>) => (start:Coord) => boolean} */
function makeInLoop (graph, pipeCoords) {
  return function inLoop (start) {
    const [r, c] = start

    let i = 0
    let pc // previous character
    for (let dc = c + 1; dc < graph[0].length; dc += 1) {
      if (!pipeCoords.has([r, dc].toString())) continue
      const ch = graph[r][dc]
      if (ch === '-') continue
      if (pc === 'F' && ch === 'J') continue
      if (pc === 'L' && ch === '7') continue
      i += 1
      pc = ch
    }
    return i % 2 === 1
  }
}

/** @type {(graph:Graph, pipeCoords:Set<string>) => number} */
function partTwo (graph, pipeCoords) {
  // too high -> 501
  let i = 0

  const inLoop = makeInLoop(graph, pipeCoords)

  for (let r = 0; r < graph.length; r += 1) {
    for (let c = 0; c < graph[0].length; c += 1) {
      const coord = [r, c].toString()
      if (!pipeCoords.has(coord) && inLoop([r, c])) {
        i += 1
      }
    }
  }
  return i
}
