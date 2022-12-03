const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = [
    data.slice(0, data.length / 2), // .split(''),
    data.slice(data.length / 2), // .split(''),
  ]

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const prio = s => (s === s.toUpperCase())
  ? s.charCodeAt(0) - 'A'.charCodeAt(0) + 27
  : s.charCodeAt(0) - 'a'.charCodeAt(0) + 1

function partOne (lines) {
  return R.sum(
    R.map(
      R.pipe(
        ([left, right]) => left.split('').find(c => right.includes(c)),
        prio,
      ),
    )(lines))
}

function partTwo (lines) {
  return 'todo'
}
