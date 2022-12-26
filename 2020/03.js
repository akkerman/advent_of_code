const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data.split('')

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  const repeat = lines[0].length
  let trees = 0
  let row = 0
  let col = 0

  while (row < lines.length) {
    col = (col + 3) % repeat
    row += 1
    if (row < lines.length && lines[row][col] === '#') {
      trees += 1
    }
  }
  console.log(row)
  return trees
}

function partTwo (lines) {
  return 'todo'
}
