const { parse, add, reduce } = require('./18-Snailfish-lib.js')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const pairs = []
const lines = []

rl.on('line', data => {
  const line = eval(data) // eslint-disable-line 
  const pair = parse(line)

  lines.push(line)
  pairs.push(pair)
})

rl.on('close', () => {
  console.log('partOne', partOne(pairs))
  console.log('partTwo', partTwo(lines))
})

function partOne (pairs) {
  let result

  for (const pair of pairs) {
    if (!result) {
      result = pair
      continue
    }
    result = reduce(add(result, pair))
  }
  return result.magnitude()
}

function partTwo (lines) {
  let maxMagnitude = 0

  for (const line1 of lines) {
    for (const line2 of lines) {
      const result = reduce(add(parse(line1), parse(line2)))
      maxMagnitude = Math.max(maxMagnitude, result.magnitude())
    }
  }
  return maxMagnitude
}
