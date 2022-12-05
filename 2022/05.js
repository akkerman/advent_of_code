const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const instructions = []
const stack1 = [null]
const stack2 = [null]

let readStack = true

rl.on('line', data => {
  if (data === '') {
    readStack = false
    return
  }

  if (readStack) {
    const line = data.split('')
    stack1.push(line)
    stack2.push([...line])
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
  console.log('partOne', partOne(stack1, instructions))
  console.log('partTwo', partTwo(stack2, instructions))
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

function partTwo (stack, instructions) {
  for (const [move, from, to] of instructions) {
    const lifted = []
    for (let i = 0; i < move; i += 1) {
      const elem = stack[from].pop()
      lifted.push(elem)
    }
    stack[to] = stack[to].concat(lifted.reverse())
  }
  return stack.slice(1).map(s => s.pop()).join('')
}
