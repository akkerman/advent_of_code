const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

function main () {
  const lines = []

  let r = 0
  let start

  rl.on('line', line => {
    line = line.split('')
    lines.push(line)

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
    console.log('partOne', partOne(lines, start))
    console.log('partTwo', partTwo(lines, start))
  })
}

main()

const isInside = graph => ([r, c]) => graph[r] && graph[r][c]

function getNeighbours (graph, node) {
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
    case 'S': // in input
    case 'L': // is a 90-degree bend connecting north and east.
      neighbours = [[r - 1, c], [r, c + 1]]
      break
    case 'J': // is a 90-degree bend connecting north and west.
      neighbours = [[r - 1, c], [r, c - 1]]
      break
    case '7': // is a 90-degree bend connecting south and west.
      neighbours = [[r, c - 1], [r + 1, c]]
      break
    // case 'S': // in example
    case 'F': // is a 90-degree bend connecting south and east.
      neighbours = [[r, c + 1], [r + 1, c]]
      break
    default:
      // '.' of 'S'
      neighbours = []
  }

  return neighbours.filter(isInside(graph)).map(coord => ({ coord, cost: 1, c: graph[coord[0]][coord[1]] }))
}

let pipes
function search (graph, start) {
  const queue = [{ coord: start, cost: 0 }]
  const visited = {
    nodes: new Set(),
    has: node => visited.nodes.has(node.coord.toString()),
    add: node => visited.nodes.add(node.coord.toString()),
  }

  let cost = 0

  while (true) {
    const node = queue.shift()

    if (!node) break

    const neighbours = getNeighbours(graph, node)
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
  pipes = visited.nodes
  return cost
}

function intersectsHorizontal (graph, start) {
  const [r, c] = start

  let i = 0
  let pc // previous character
  for (let dc = c + 1; dc < graph[0].length; dc += 1) {
    if (!pipes.has([r, dc].toString())) continue
    const ch = graph[r][dc]
    if (ch === '-') continue
    if (pc === 'F' && ch === 'J') continue
    if (pc === 'L' && ch === '7') continue
    i += 1
    pc = ch
  }
  return i
}

function intersectVertical (graph, start) {
  const [r, c] = start

  let i = 0
  let pc // previous character
  for (let dr = r + 1; dr < graph.length; dr += 1) {
    if (!pipes.has([dr, c].toString())) continue
    const ch = graph[dr][c]
    if (ch === '|') continue
    if (pc === '7' && ch === 'L') continue
    if (pc === 'F' && ch === 'J') continue
    i += 1
    pc = ch
  }
  return i
}

function isInsidePipes (i) {
  return i % 2 === 1
}

function partOne (graph, start) {
  return search(graph, start)
}

function partTwo (graph) {
  // too high -> 501
  let i = 0

  for (let r = 0; r < graph.length; r += 1) {
    for (let c = 0; c < graph[0].length; c += 1) {
      const coord = [r, c].toString()
      if (!pipes.has(coord)) {
        const ih = intersectsHorizontal(graph, [r, c])
        const iv = intersectVertical(graph, [r, c])
        if (isInsidePipes(ih)) {
          if (!isInsidePipes(iv)) {
            log([r, c], ih, iv)
            throw new Error('different answers from H and V')
          }
          i += 1
        }
      }
    }
  }
  return i
}
