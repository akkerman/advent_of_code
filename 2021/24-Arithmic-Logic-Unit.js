const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const programParts = []
const lines = []

rl.on('line', data => {
  const line = data.split(' ')
  if (line[0] === 'inp') { programParts.push([]) }

  programParts[programParts.length - 1].push(line)
  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne())
  console.log('partTwo', partTwo())
})

const initialize = () => ({
  w: 0,
  x: 0,
  y: 0,
  z: 8,
})

function createProgram (instructions) {
  const fns = {
    add: (a, b) => a + b,
    mul: (a, b) => a * b,
    div: (a, b) => b === 0 ? a : Math.floor(a / b),
    mod: (a, b) => (a < 0 || b <= 0) ? a : a % b,
    eql: (a, b) => a === b ? 1 : 0,
  }

  return function calculate (monad) {
    return instructions.reduce(process, initialize())

    function process (variables, instruction) {
      if (instruction[0] === 'inp') {
        variables[instruction[1]] = monad.shift()
        return variables
      }

      const [op, left, right] = instruction
      const leftValue = variables[left]
      const rightVar = variables[right]
      const rightValue = (rightVar !== undefined) ? rightVar : parseInt(right)

      variables[left] = fns[op](leftValue, rightValue)

      return variables
    }
  }
}

function captureVariableParts () {
  return programParts.map(xs => {
    const divz = xs[4][2]
    const addx = xs[5][2]
    const addy = xs[15][2]
    return [divz, addx, addy].map(Number)
  })
}

function partOne () {
  const max = Array(14).fill(null)
  const min = Array(14).fill(null)

  const variables = captureVariableParts()
  const backtrack = []
  for (let idx = 0; idx < variables.length; idx += 1) {
    const [divz, addx, addy] = variables[idx]
    if (divz === 1) {
      backtrack.push([idx, addy])
      continue
    }

    const [prevIdx, prevAddy] = backtrack.pop()
    const diff = prevAddy + addx

    if (diff < 0) {
      max[idx] = 9 + diff
      max[prevIdx] = 9

      min[prevIdx] = 1 - diff
      min[idx] = 1
    } else {
      max[idx] = 9
      max[prevIdx] = 9 - diff

      min[prevIdx] = 1
      min[idx] = 1 + diff
    }
  }

  return {
    max: max.join(''), // 98491959997994
    min: min.join(''), // 61191516111321
  }
}
function partTwo () {
  return 'todo'
}
