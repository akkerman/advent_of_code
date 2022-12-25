const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const R = require('ramda')

const lines = []

const columns = [[], [], []]

rl.on('line', data => {
  const line = data.trim().replace(/ +/g, ' ').split(' ').map(i => parseInt(i))
  lines.push(line)

  for (let i = 0; i <= 2; i += 1) {
    columns[i].push(line[i])
  }
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(columns))
})

function partOne (lines) {
  let sum = 0
  for (const line of lines) {
    const [a, b, c] = line.sort((a, b) => a - b)
    if (a + b > c) sum += 1
  }
  return sum
}

function partTwo (columns) {
  const lines = R.splitEvery(3, [].concat(...columns))

  return partOne(lines)
}
