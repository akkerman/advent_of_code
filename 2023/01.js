const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const sum = (a, b) => a + b

function partOne (lines) {
  return lines.map(line => {
    const res = line.replace(/[a-z]/g, '').split('')
    const first = res[0]
    const last = res[res.length - 1]
    return parseInt(`${first}${last}`)
  }).reduce(sum)
}

const num = {
  one: 1,
  two: 2,
  three: 3,
  four: 4,
  five: 5,
  six: 6,
  seven: 7,
  eight: 8,
  nine: 9,

  1: 1,
  2: 2,
  3: 3,
  4: 4,
  5: 5,
  6: 6,
  7: 7,
  8: 8,
  9: 9,
}

const stuf = Object.entries(num)

function partTwo (lines) {
  return lines.map(line => {
    const occur = []
    for (const [str, num] of stuf) {
      const indices = findOccurences(str, line)

      for (const idx of indices) {
        occur.push([num, idx])
      }
    }
    occur.sort(([, i1], [, i2]) => i1 - i2)
    return occur.map(([n]) => n)
  })
    .map(occur => parseInt(`${occur[0]}${occur[occur.length - 1]}`))
    .reduce(sum)
}

function findOccurences (str, line) {
  const rgx = new RegExp(str, 'gi')
  const indices = []
  let result = {}
  while ((result = rgx.exec(line))) {
    indices.push(result.index)
  }
  return indices
}
