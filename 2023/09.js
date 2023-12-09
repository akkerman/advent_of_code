const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

function main () {
  const lines = []

  rl.on('line', line => {
    line = line.split(' ').map(int)

    lines.push(line)
  })

  rl.on('close', () => {
    console.log('partOne', partOne(lines))
    console.log('partTwo', partTwo(lines))
  })
}

main()

function sequencer (history) {
  const last = []
  let h = [...history]
  while (true) {
    last.push(h[h.length - 1])

    const nh = []
    for (let i = 1; i < h.length; i += 1) {
      nh.push(h[i] - h[i - 1])
    }
    h = nh
    if (h.every(n => n === 0)) break
  }
  return last.reduce(sum)
}

function partOne (lines) {
  return lines
    // .map(tap(log))
    .map(sequencer).reduce(sum)
}

function partTwo (lines) {
  return 'todo'
}
