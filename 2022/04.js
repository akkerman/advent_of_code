const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data.split(/[-,]/).map(i => parseInt(i))

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const contains = ([a1, a2, b1, b2]) => (a1 <= b1 && b2 <= a2) || (b1 <= a1 && a2 <= b2) ? 1 : 0

const overlap = ([a1, a2, b1, b2]) => (a1 <= b1 && a2 >= b1) || (b1 <= a1 && b2 >= a1) ? 1 : 0

function partOne (lines) {
  return R.sum(R.map(contains, lines))
}

function partTwo (lines) {
  return R.sum(R.map(overlap, lines))
}
