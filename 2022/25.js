const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const { snafu2dec, dec2snafu } = require('./25-snafu.js')

const lines = []

rl.on('line', data => {
  const line = snafu2dec(data)

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
})

function partOne (lines) {
  return dec2snafu(lines.reduce((a, b) => a + b))
}
