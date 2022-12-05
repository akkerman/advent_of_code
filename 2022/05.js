const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const instructions = []
const stack = [null]

let readStack = true

rl.on('line', data => {
  if (data === '') {
    readStack = false
    return
  }

  if (readStack) {
    const line = data.split('')
    stack.push(line)
    return
  }

  const line = data
    .replace('move ', '')
    .replace('from ', '')
    .replace('to ', '')
    .split(/ +/)
    .map(i => Number
      .parseInt(i))

  instructions.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(stack, instructions))
  console.log('partTwo', partTwo(stack, instructions))
})

function partOne (stack, instructions) {
  for (const [move, from, to] of instructions) {
    for (let i = 0; i < move; i += 1) {
      const elem = stack[from].pop()
      stack[to].push(elem)
    }
  }
  return stack.slice(1).map(s => s.pop()).join('')
}

function partTwo (lines) {
  return 'todo'
}
