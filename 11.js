const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on("line", data => {
  const line = data.split('').map(i => Number.parseInt(i))

  lines.push(line)
})

let maxRow = 0 
let maxCol = 0
rl.on('close', () => {
  maxRow = lines.length-1
  maxCol = lines[0].length-1
  console.log(lines.length, 'lines in file of length')

  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const pp = lines => {
  for (const line of lines) {
    console.log(line.map(o=>o===0?'.':o).join(' '))
  }
}

const inc = a => a + 1

function nextStep(lines) {
  const next = lines.map(line => line.map(inc))
  return next
}

const cell = (r,c) => [r,c]
const withinCavern = ([r,c]) => 0<=r && 0<=c && r<=maxRow && c<=maxCol

function getNeighbours(row, col) {
  const neighbours = []
  for (let r = row-1; r<=row+1; r+=1) {
    for (let c = col-1; c<=col+1; c+=1) {
      if (r==row && c===col) continue
      neighbours.push(cell(r,c))
    }
  }
  return neighbours.filter(withinCavern)
}
function flashersLeft(lines) {
  for (let row = 0; row<=maxRow; row+=1) {
    for (let col = 0; col<=maxCol; col+=1){
      if (9 < lines[row][col]) {
        return true
      }
    }
  }
  return false
}

function flash(lines) {
  for (let row = 0; row<=maxRow; row+=1) {
    for (let col = 0; col<=maxCol; col+=1){
      if (9 < lines[row][col]) {
        for (let [r,c] of getNeighbours(row,col)) {
          if (lines[r][c]>0)
            lines[r][c]+=1
        }
        lines[row][col]=0
      }
    }
  }
  if (!flashersLeft(lines)) {
    return lines
  }
  return flash(lines)
}

function partOne(lines) {
  let step = lines
  let sum = 0
  for (let nr = 1; nr <= 100; nr +=1) {
    step = flash(nextStep(step))

    const flashers = step.flatMap(_=>_).filter(o=>o===0)
    // console.log(flashers)
    sum += flashers.length
  }
  return sum
}

function partTwo(lines) {
  return 'todo'
}
