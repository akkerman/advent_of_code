const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

function parse (monkeys, line) {
  const [name, yell] = line.split(': ')

  const num = parseInt(yell)
  if (!isNaN(num)) {
    monkeys[name] = { name, num }
  } else {
    const [left, operation, right] = yell.split(' ')
    monkeys[name] = { name, left, operation, right }
  }
  return monkeys
}

rl.on('line', data => {
  lines.push(data)
})

rl.on('close', () => {
  const monkeys = lines.reduce(parse, {})
  console.log('partOne', partOne(monkeys))

  console.log('partTwo', partTwo(lines))
})

function calc (m, left, right) {
  return eval(`${left.num} ${m.operation} ${right.num}`) // eslint-disable-line 
}

function partOne (monkeys) {
  const queue = [monkeys.root]

  while (true) {
    const m = queue.pop()
    const left = monkeys[m.left]
    const right = monkeys[m.right]
    if (left.num && right.num) {
      m.num = calc(m, left, right)
      if (m.name === 'root') return m.num
    } else {
      queue.push(m)
      if (!left.num) queue.push(left)
      if (!right.num) queue.push(right)
    }
  }
}

function partTwo (lines) {
  // spelletje hoger/lager -> binary search

  let min = 2
  let max = 276156919469632
  let guess = max / 2

  while (min < max) {
    const monkeys = lines.reduce(parse, {})
    monkeys.root.operation = '==='
    monkeys.humn.num = guess
    if (partOne(monkeys)) {
      return monkeys.humn.num
    }
    if (monkeys[monkeys.root.left].num < monkeys[monkeys.root.right].num) {
      const tmp = (min + guess) / 2
      max = guess
      guess = tmp
    } else {
      const tmp = (guess + max) / 2
      min = guess
      guess = tmp
    }
  }
  return 'oeps'
}
