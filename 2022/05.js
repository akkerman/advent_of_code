const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const R = require('ramda')

const instructions = []
const stack = []

let readStack = true

const instructionRegex = /move (\d+) from (\d+) to (\d+)/

rl.on('line', data => {
  if (data === '') {
    readStack = false
    stack.pop() // remove the numbers
    return
  }

  if (readStack) {
    const line = data
    stack.push(line)
    return
  }

  const line = data.match(instructionRegex).slice(1, 4)
  instructions.push(line)
})

function parseStackRotate (stack) {
  const newStack = []
  const size = stack[0].length
  for (const line of stack) {
    for (let i = 0; i < size; i += 1) {
      const elem = line[i]
      if (' []'.includes(elem)) continue
      if (!newStack[i]) newStack[i] = []
      newStack[i].unshift(elem)
    }
  }

  return [[], ...newStack.filter(a => a.length)]
}

const clean = s => s.replace(/[\[ \]]/g, '') // eslint-disable-line 

const parseStackRamda = stack => [[], ...R.pipe(
  R.map(R.pipe(R.splitEvery(4), R.map(clean))),
  R.pipe(R.reverse, R.transpose),
  R.map(R.filter(R.pipe(R.isEmpty, R.not))),
)(stack)]

const parseStack = parseStackRamda

rl.on('close', () => {
  console.log('partOne', partOne(parseStack(stack), instructions))
  console.log('partTwo', partTwo(parseStack(stack), instructions))
})

function partOne (stack, instructions) {
  for (const [move, from, to] of instructions) {
    for (let i = 0; i < move; i += 1) {
      const elem = stack[from].pop()
      stack[to].push(elem)
    }
  }
  return stack.map(s => s.pop()).join('')
}

function partTwo (stack, instructions) {
  for (const [move, from, to] of instructions) {
    stack[to].push(...stack[from].splice(-move))
  }
  return stack.map(s => s.pop()).join('')
}
