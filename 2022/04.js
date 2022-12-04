const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data.replaceAll('-', ',').split(',').map(i => parseInt(i))

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function contains ([a1, a2, b1, b2]) {
  if (
    (a1 <= b1 && b2 <= a2) ||
    (b1 <= a1 && a2 <= b2)
  ) return 1

  return 0
}

function partOne (lines) {
  return R.sum(R.map(contains, lines))
}

function partTwo (lines) {
  return 'todo'
}
