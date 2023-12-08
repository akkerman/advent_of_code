const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = fn => args => { fn(args); return args } // eslint-disable-line 
const log = console.log // eslint-disable-line
const sum = (a, b) => a + b // eslint-disable-line
const { lcm } = require('../utils.js')

let instructions
const direction = { L: 0, R: 1 }
const network = {}

rl.on('line', data => {
  if (!instructions) { instructions = data; return }
  if (data === '') return

  const [node, LR] = data.replace(/[()]/g, '').split(' = ')
  network[node] = LR.split(', ')
})

rl.on('close', () => {
  console.log('partOne', partOne(network, instructions))
  console.log('partTwo', partTwo(network, instructions))
})

function * makeNextInstruction (instructions) {
  const I = instructions.split('')
  let idx = 0
  while (true) {
    yield I[idx]
    idx += 1
    if (idx === I.length) idx = 0
  }
}

function partOne (network, instructions) {
  const nextInstruction = makeNextInstruction(instructions)

  let steps = 0
  let node = 'AAA'
  for (const I of nextInstruction) {
    steps += 1
    node = network[node][direction[I]]

    if (node === 'ZZZ') return steps
  }
}

function partTwo (network, instructions) {
  const nodes = Object.keys(network).filter(n => n[2] === 'A')

  const nums = {}

  for (const start of nodes) {
    nums[start] = []
    let node = start
    let i = 0
    let steps = 0
    const nextInstruction = makeNextInstruction(instructions)

    for (const I of nextInstruction) {
      steps += 1
      node = network[node][direction[I]]

      if (node[2] === 'Z') {
        nums[start].push(steps)
        node = start
        steps = 0
        i += 1
        if (i >= 1) break
      }
    }
  }
  return Object.values(nums).map(a => a[0]).reduce(lcm)
}
