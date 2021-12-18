const { parse, add, reduce } = require('./18-Snailfish-lib.js')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const pairs = []

rl.on('line', data => {
  const pair = parse(eval(data)) // eslint-disable-line 

  pairs.push(pair)
})

rl.on('close', () => {
  console.log('partOne', partOne(pairs))
  console.log('partTwo', partTwo(pairs))
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
  return 'todo'
}
