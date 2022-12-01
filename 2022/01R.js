const R = require('ramda')

const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data === '' ? 0 : Number.parseInt(data)

  lines.push(line)
})

rl.on('close', () => {
  const sorted = R.pipe(
    R.splitWhenever(R.equals(0)),
    R.map(R.sum),
    R.sort((a, b) => b - a),
  )(lines)

  console.log('partOne', partOne(sorted))
  console.log('partTwo', partTwo(sorted))
})

function partOne (lines) {
  return R.head(lines)
}

function partTwo (lines) {
  return R.pipe(
    R.take(3),
    R.sum,
  )(lines)
}
