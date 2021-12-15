const Heap = require('heap')

const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let lines = []
let maxRow
let maxCol

rl.on("line", data => {
  const line = data.split('').map(i => Number.parseInt(i))

  lines.push(line)
})

rl.on('close', () => {
  maxRow = lines.length - 1
  maxCol = lines[0].length - 1
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})


const cell = (row,col) => lines[row][col]
const coordValue = (row,col, value) => {
  if (value === undefined) value = cell(row,col)
  return {row, col, value, label:`${row}_${col}`}
}

const getNeighboursWithCoord = makeGetNeigbours(coordValue)

function makeGetNeigbours (cell) {
    return function getNeighbours(row, col) {
      const neighbours = []
      if (row > 0) {
        neighbours.push(cell(row-1, col))
      }
      if (row < maxRow) {
        neighbours.push(cell(row+1, col))
      }
      if (col > 0) {
        neighbours.push(cell(row, col-1))
      }
      if (col < maxCol) {
        neighbours.push(cell(row, col+1))
      }
      return neighbours
    }
}

const visited = new Set()
const prio = new Heap((a, b) =>  a.value - b.value)

function partOne() {
  visited.clear()
  prio.length = 0
  prio.push(coordValue(0,0,0))

  while (true) {
    const {row, col, value, label} = prio.pop()

    if (visited.has(label)) continue
    if (row === maxRow && col == maxCol) return value
    visited.add(label)

    for (const nb of getNeighboursWithCoord(row, col)) {
      if (visited.has(nb.label)) continue
      prio.push(coordValue(nb.row, nb.col, value + nb.value))
    }
  }
  return 'oeps'
}

function partTwo() {
  return 'todo'
}
