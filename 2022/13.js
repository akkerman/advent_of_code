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
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (pairs) {
  for (const pair of pairs) {
    const { left, right } = pair
    pair.correct = check(left, right)
  }
  return pairs.filter(p => p.correct).reduce((acc, pair) => acc + pair.idx, 0)
}

function partTwo (pairs) {
  const divider2 = [[2]]
  const divider6 = [[6]]

  const packets = [divider2, divider6]
  for (const pair of pairs) {
    const { left, right } = pair
    packets.push(left)
    packets.push(right)
  }

  packets.sort((a, b) => check(a, b) ? -1 : 1)

  return (1 + packets.indexOf(divider2)) * (1 + packets.indexOf(divider6))
}
