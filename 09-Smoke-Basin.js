
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
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function cell(row,col) {
  return lines[row][col]
}

function getNeighbours(row, col) {
  const nums = []
  if (row > 0) {
    nums.push(cell(row-1, col))
  }
  if (row < maxRow) {
    nums.push(cell(row+1, col))
  }
  if (col > 0) {
    nums.push(cell(row, col-1))
  }
  if (col < maxCol) {
    nums.push(cell(row, col+1))
  }
  return nums
}

function isLow(row, col) {
  const current = cell(row,col)
  const neighbours = getNeighbours(row,col)

  return current < Math.min(...neighbours)
}

function partOne(lines) {
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

function partTwo(lines) {
  return 'todo'
}
