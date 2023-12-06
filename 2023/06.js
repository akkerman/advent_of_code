const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = fn => args => { fn(args); return args } // eslint-disable-line 
const log = console.log // eslint-disable-line
const sum = (a, b) => a + b // eslint-disable-line 

const lines = []

rl.on('line', data => {
  const line = data
    .replace(/.*: */, '')
    .trim()
    .split(/ +/)
    .map(s => parseInt(s.trim()))

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  const pairs = R.zip(...lines)
  return pairs.map(calculateWins)
    .reduce(R.multiply)
}

function calculateWins ([time, record]) {
  let wins = 0
  for (let wait = 0; wait <= time; wait += 1) {
    const distance = (time - wait) * wait
    if (distance > record) {
      wins += 1
    }
  }
  return wins
}

function partTwo (lines) {
  return 'todo'
}
