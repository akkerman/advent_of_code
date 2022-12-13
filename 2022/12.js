const Heap = require('heap')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

let startRow
let startCol
let endRow
let endCol

let row = 0
let maxRow = 0
let maxCol = 0

const delta = 'a'.charCodeAt(0) - 1
const toInt = s => {
  switch (s) {
    case 'S': return 1
    case 'E': return 26
    default: return s.charCodeAt(0) - delta
  }
}

rl.on('line', line => {
  let col = line.indexOf('S')
  if (col > -1) {
    startRow = row
    startCol = col
  }
  col = line.indexOf('E')
  if (col > -1) {
    endRow = row
    endCol = col
  }

  lines.push(line.split('').map(toInt))
  row += 1
})

rl.on('close', () => {
  maxCol = lines[0].length - 1
  maxRow = lines.length - 1

  let start = new Date()
  console.log('partOne', partOne())
  console.log(new Date().getTime() - start.getTime(), 'ms')
  start = new Date()
  console.log('partTwo', partTwo())
  console.log(new Date().getTime() - start.getTime(), 'ms')
  start = new Date()
  // console.log('partTwoBrute', partTwoBrute())
  // console.log(new Date().getTime() - start.getTime(), 'ms')
  // start = new Date()
})

const manhattanDistance = (row, col) => Math.abs(row - endRow) + Math.abs(col - endCol)
const euclideanDistance = (row, col) => Math.hypot(row - endRow, col - endCol)
const coordWithSteps = (row, col, steps) => {
  if (steps === undefined) steps = 1
  return { row, col, height: lines[row][col], distance: manhattanDistance(row, col), steps, label: `${row}_${col}` }
}

const makeIsReachableGoingUp = (refRow, refCol) => {
  const maxHeight = lines[refRow][refCol] + 1
  return (row, col) => lines[row][col] <= maxHeight
}

const makeIsReachableGoingDown = (refRow, refCol) => {
  const minHeight = lines[refRow][refCol] - 1
  return (row, col) => minHeight <= lines[row][col]
}

const getNeighboursWithCoord = makeGetNeigbours(coordWithSteps, makeIsReachableGoingUp)
const getNeighboursDownWithCoord = makeGetNeigbours(coordWithSteps, makeIsReachableGoingDown)

function makeGetNeigbours (cell, makeIsReachable) {
  return function getNeighbours (row, col) {
    const isReachable = makeIsReachable(row, col)
    const neighbours = []
    if (row > 0 && isReachable(row - 1, col)) {
      neighbours.push(cell(row - 1, col))
    }
    if (row < maxRow && isReachable(row + 1, col)) {
      neighbours.push(cell(row + 1, col))
    }
    if (col > 0 && isReachable(row, col - 1)) {
      neighbours.push(cell(row, col - 1))
    }
    if (col < maxCol && isReachable(row, col + 1)) {
      neighbours.push(cell(row, col + 1))
    }
    return neighbours
  }
}

const visited = new Set()
// const prio = new Heap((a, b) => a.steps - b.steps)
const prio = new Heap((a, b) => (a.steps + a.distance) - (b.steps + b.distance))

function partOne () {
  visited.clear()
  prio.length = 0
  prio.push(coordWithSteps(startRow, startCol, 0))

  let pops = 0

  while (true) {
    const { row, col, steps, label } = prio.pop()
    pops += 1

    if (row === endRow && col === endCol) return { steps, pops }
    if (visited.has(label)) continue
    visited.add(label)

    for (const nb of getNeighboursWithCoord(row, col)) {
      if (visited.has(nb.label)) continue
      prio.push(coordWithSteps(nb.row, nb.col, steps + nb.steps))
    }
  }
}

function partTwoBrute () {
  let min = Number.MAX_SAFE_INTEGER

  for (let row = 0; row < lines.length; row += 1) {
    for (let col = 0; col < lines[0].length; col += 1) {
      if (lines[row][col] === 1) {
        startRow = row
        startCol = col
        min = Math.min(min, partOne().steps)
      }
    }
  }
  return min
}

function partTwo () {
  visited.clear()
  prio.length = 0
  prio.push(coordWithSteps(endRow, endCol, 0))

  let pops = 0
  while (true) {
    const { row, col, steps, label } = prio.pop()
    pops += 1

    if (lines[row][col] === 1) return { steps, pops }
    if (visited.has(label)) continue
    visited.add(label)

    for (const nb of getNeighboursDownWithCoord(row, col)) {
      if (visited.has(nb.label)) continue
      prio.push(coordWithSteps(nb.row, nb.col, steps + nb.steps))
    }
  }
}
