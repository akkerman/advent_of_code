const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const check = require('./13-check.js')

const lines = []

let left = null
let right = null
let idx = 1

rl.on('line', data => {
  if (data === '') {
    lines.push({ left, right, idx, correct: false })
    left = null
    right = null
    idx += 1
    return
  }

  if (!left) {
    left = left || eval(data)
    return
  }
  if (!right) {
    right = right || eval(data)
  }
})

rl.on('close', () => {
  console.log('partOne', partOne(lines)) // too low: 679 977
  // console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  for (const pair of lines) {
    const { left, right } = pair
    pair.correct = check(left, right)
    console.log(pair.correct)
  }
  return lines.filter(p => p.correct).reduce((acc, pair) => acc + pair.idx, 0)
}

function partTwo (lines) {
  return 'todo'
}
