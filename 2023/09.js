const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const sum = (a,b) => a+b // eslint-disable-line
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
  return lines.map(sequencer).reduce(sum)
}

function sequencer2 (history) {
  const fst = []
  let h = [...history]
  while (true) {
    fst.push(h[0])

    const nh = []
    for (let i = 1; i < h.length; i += 1) {
      nh.push(h[i] - h[i - 1])
    }
    h = nh
    if (h.every(n => n === 0)) break
  }

  const zero = [0]
  for (let i = fst.length - 1; i >= 0; i -= 1) {
    zero.push(fst[i] - zero[zero.length - 1])
  }

  return zero[zero.length - 1]
}

function partTwo (lines) {
  return lines.map(sequencer2).reduce(sum)
}
