
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data.split('')

  lines.push(line)
})

let maxRow
let maxCol
let empty
rl.on('close', () => {
  maxRow = lines.length - 1
  maxCol = lines[0].length - 1
  empty = Array.from({ length: maxRow + 1 }, () => Array(maxCol + 1).fill('.'))

  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function pp (floor) { // eslint-disable-line 
  for (const muck of floor) {
    console.log(muck.join(''))
  }
  console.log(' ')
}

function left (from, to) {
  let moved = false
  for (let row = 0; row <= maxRow; row += 1) {
    for (let col = 0; col <= maxCol; col += 1) {
      const neighbourCol = col === maxCol ? 0 : col + 1
      if (from[row][col] === '>' && from[row][neighbourCol] === '.') {
        moved = true
        to[row][neighbourCol] = '>'
      } else if (from[row][col] !== '.') {
        to[row][col] = from[row][col]
      }
    }
  }
  // console.log('left', moved)
  return moved
}

function down (from, to) {
  let moved = false
  for (let row = 0; row <= maxRow; row += 1) {
    for (let col = 0; col <= maxCol; col += 1) {
      const neighbourRow = row === maxRow ? 0 : row + 1
      if (from[row][col] === 'v' && from[neighbourRow][col] === '.') {
        moved = true
        to[neighbourRow][col] = 'v'
      } else if (from[row][col] !== '.') {
        to[row][col] = from[row][col]
      }
    }
  }
  // console.log('down', moved)
  return moved
}

function clear (field) {
  return field.map(arr => arr.fill('.'))
}

function partOne () {
  let steps = 0
  let from = lines
  let to = empty
  let tmp

  while (true) {
    // console.log(`After ${steps} steps:`)
    // pp(from)

    const leftMoved = left(from, to)
    tmp = from; from = to; to = clear(tmp)

    const downMoved = down(from, to)
    tmp = from; from = to; to = clear(tmp)

    steps += 1
    if (!leftMoved && !downMoved) break
  }
  return steps
}

function partTwo (lines) {
  return 'todo'
}
