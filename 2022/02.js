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

const LOSE_FROM = {
  [R]: S,
  [S]: P,
  [P]: R,
}

const WIN_FROM = {
  [S]: R,
  [P]: S,
  [R]: P,
}

function roundScore ([opponent, self]) {
  if (opponent === self) { return 3 }
  if (LOSE_FROM[self] === opponent) { return 6 }

  return 0
}

function pickSelf ([opponent, result]) {
  switch (result) {
    case 'Y': return opponent
    case 'X': return LOSE_FROM[opponent]
    case 'Z': return WIN_FROM[opponent]
  }
}

rl.on('line', data => {
  const line = data.split(' ')

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  return lines
    .map(([abc, xyz]) => [OPPONENT[abc], SELF[xyz]])
    .map(([opponent, self]) => roundScore([opponent, self]) + SCORE[self])
    .reduce((a, b) => a + b)
}

function partTwo (lines) {
  return lines
    .map(([abc, result]) => [OPPONENT[abc], result])
    .map(([opponent, result]) => [opponent, pickSelf([opponent, result])])
    .map(([opponent, self]) => roundScore([opponent, self]) + SCORE[self])
    .reduce((a, b) => a + b)
}
