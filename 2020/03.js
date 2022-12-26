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

function solve (lines, dCol, dRow) {
  const repeat = lines[0].length
  let trees = 0
  let row = 0
  let col = 0

  while (row < lines.length) {
    col = (col + dCol) % repeat
    row += dRow
    if (row < lines.length && lines[row][col] === '#') {
      trees += 1
    }
  }
  return trees
}

function partOne (lines) {
  return solve(lines, 3, 1)
}

function partTwo (lines) {
  const slopes = [
    [1, 1],
    [3, 1],
    [5, 1],
    [7, 1],
    [1, 2],
  ]

  let prod = 1

  for (const [r, c] of slopes) {
    const result = solve(lines, r, c)
    prod *= result
  }

  return prod
}
