const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const monkeys = []

rl.on('line', data => {
  const [name, yell] = data.split(': ')

  const num = parseInt(yell)
  if (isNaN(num)) {
    const [left, operation, right] = yell.split(' ')
    monkeys[name] = { name, left, operation, right }
  } else {
    monkeys[name] = { name, num }
  }
})

rl.on('close', () => {
  console.log('partOne', partOne(monkeys))
  console.log('partTwo', partTwo(monkeys))
})

function calc (m, left, right) {
  return eval(`${left.num} ${m.operation} ${right.num}`)
}

function partOne (monkeys) {
  const queue = [monkeys.root]

  while (true) {
    const m = queue.pop()
    if (!m.num) {
      const left = monkeys[m.left]
      const right = monkeys[m.right]
      if (left.num && right.num) {
        m.num = calc(m, left, right)
        if (m.name === 'root') return m.num
      } else {
        queue.push(m)
        queue.push(left)
        queue.push(right)
      }
    }
  }
}

function partTwo (lines) {
  return 'todo'
}
