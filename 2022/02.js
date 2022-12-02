const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

const R = 'Rock'
const P = 'Paper'
const S = 'Scissors'

const OPPONENT = {
  A: R,
  B: P,
  C: S,
}

const SELF = { // Self
  X: R,
  Y: P,
  Z: S,
}

const SCORE = {
  [R]: 1,
  [P]: 2,
  [S]: 3,
}

const WINS = {
  [R]: S,
  [S]: P,
  [P]: R,
}

function roundScore ([opponent, self]) {
  if (opponent === self) { return 3 }
  if (WINS[self] === opponent) { return 6 }

  return 0
}

rl.on('line', data => {
  const [opponent, self] = data.split(' ')

  lines.push([OPPONENT[opponent], SELF[self]])
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  return lines
    .map(([opponent, self]) => roundScore([opponent, self]) + SCORE[self])
    .reduce((a, b) => a + b)

  return 'todo'
}

function partTwo (lines) {
  return 'todo'
}
