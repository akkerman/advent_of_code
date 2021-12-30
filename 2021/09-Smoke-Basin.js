
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []
let maxRow
let maxCol

rl.on("line", data => {
  const line = data.split('').map(i=>Number.parseInt(i))
  lines.push(line)
  maxRow = lines.length - 1
  maxCol = lines[0].length - 1
})

rl.on('close', () => {
  console.log({maxRow, maxCol})
  console.log('partOne', partOne())
  console.log('partTwo', partTwo())
})

const cell = (row,col) => lines[row][col]
const coordValue = (row,col) => ({row, col, value: cell(row,col)})

const getNeighbours = makeGetNeigbours(cell)
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

function isLow(row, col) {
  const current = cell(row,col)
  const neighbours = getNeighbours(row,col)

  return current < Math.min(...neighbours)
}

function coordInBassin(coord, bassin) {
  return !!bassin.find(({row,col}) => row === coord.row && col === coord.col)
}

function createBassin(row,col) {
  return solve(coordValue(row,col))

  function solve(cv, bassin=[]) {
    if (coordInBassin(cv,bassin) || cv.value === 9) return bassin
    let newBassin = bassin.concat(cv)
    const nbs = getNeighboursWithCoord(cv.row, cv.col)
    for (let nb of nbs) {
      newBassin = solve(nb, newBassin)
    }
    return newBassin
  }
}

function partOne() {
  let sum = 0
  for (let row = 0; row <= maxRow; row +=1 ) {
    for (let col = 0; col <= maxCol; col +=1 ) {
      if (isLow(row, col)) {
        sum+=cell(row, col) + 1
      }
    }
  }
  return sum
}

function partTwo() {
  const bassins = []
  outer:for (let row = 0; row <= maxRow; row +=1 ) {
    for (let col = 0; col <= maxCol; col +=1 ) {
      if (isLow(row, col)) {
        bassins.push(createBassin(row,col))
      }
    }
  }

  return bassins.map(b => b.length).sort((a,b)=>b-a).slice(0,3).reduce((a,c) => a*c,1)
}
