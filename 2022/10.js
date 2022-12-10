const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const R = require('ramda')

const lines = []

rl.on('line', data => {
  const line = data

  lines.push(line)
})

const cycleStrength = (cycles, x) => cycles * x

rl.on('close', () => {
  console.log('partOne', partOne(lines)) // incorrect: 21880
  console.log('partTwo', partTwo(lines))
})

function partOne (instructions) {
  let registerX = 1
  let cycles = 0
  let strength = 0
  for (const instruction of instructions) {
    function check () {
      cycles += 1
      if ((cycles + 20) % 40 === 0) {
        const current = cycleStrength(cycles, registerX)
        strength += current
      }
    }

    check()

    if (!instruction.includes('noop')) {
      check()
      const add = parseInt(instruction.split(' ')[1])
      registerX += add
    }
  }
  return strength
}

function partTwo (instructions) {
  const crt = []
  let registerX = 1
  let cycles = 0
  for (const instruction of instructions) {
    function check () {
      if ([registerX - 1, registerX, registerX + 1].includes(cycles % 40)) {
        crt.push('#')
      } else {
        crt.push('.')
      }
      cycles += 1
    }

    check()

    if (!instruction.includes('noop')) {
      check()
      const add = parseInt(instruction.split(' ')[1])
      registerX += add
    }
  }
  return R.map(R.join(''), R.splitEvery(40, crt))
}
