const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

function parseInstruction (instr) {
  if (!instr.includes(':')) return () => instr

  const [, name, operator, value, next] = /([xmas])([<>])(\d+):(.+)/.exec(instr)

  const v = int(value)

  if (operator === '<') {
    return (partName, partValue) => name === partName && partValue < v ? next : undefined
  }
  return (partName, partValue) => name === partName && partValue > v ? next : undefined
}

function main () {
  const instructions = {}
  const parts = []

  let instructionParsing = true
  rl.on('line', line => {
    line = line.toString()

    if (line === '') {
      instructionParsing = false
      return
    }

    if (instructionParsing) {
      const [, name, i] = /(.+){(.+)}/.exec(line)

      const instr = i.split(',')
      instructions[name] = instr.map(parseInstruction)
      return
    }

    const part = JSON.parse(
      line.replaceAll('=', ':').replace(/([xmas])/g, '"$1"'),
    )

    parts.push(part)
  })

  rl.on('close', () => {
    console.log('partOne', partOne(instructions, parts))
    console.log('partTwo', partTwo(instructions, parts))
  })
}

main()

// x: Extremely cool looking
// m: Musical (it makes a noise when you hit it)
// a: Aerodynamic
// s: Shiny

function partOne (instructions, parts) {
  function workflow (part) {
    let instructionName = 'in'

    while (!'AR'.includes(instructionName)) {
      for (const fn of instructions[instructionName]) {
        let answ
        for (const [name, value] of Object.entries(part)) {
          answ = fn(name, value)
          if (answ) {
            break
          }
        }
        if (answ) {
          instructionName = answ
          break
        }
      }
    }

    return instructionName === 'A'
  }

  return parts.filter(workflow).map(part => Object.values(part).reduce(sum)).reduce(sum)
}

function partTwo (instructions, parts) {
  return 'todo'
}
