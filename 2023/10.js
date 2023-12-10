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

  return cost
}

function partOne (graph, start) {
  // too low -> 6796
  return search(graph, start)
}

function partTwo (lines) {
  return 'todo'
}
