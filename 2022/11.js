const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let lines = []
const monkeys = []

function parseMonkey (lines) {
  let idx = 1
  return {
    items: lines[idx++].split(': ')[1].split(', ').map(i => parseInt(i)),
    operation: lines[idx++].split('new = ')[1],
    test: parseInt(lines[idx++].split('by ')[1]),
    ifTrue: parseInt(lines[idx++].split('monkey ')[1]),
    ifFalse: parseInt(lines[idx++].split('monkey ')[1]),
    inspections: 0,
  }
}

rl.on('line', data => {
  const line = data

  if (line === '') {
    monkeys.push(parseMonkey(lines))
    lines = []
  } else {
    lines.push(line)
  }
})

rl.on('close', () => {
  console.log('partOne', partOne(monkeys))
  console.log('partTwo', partTwo(monkeys))
})

function partOne (monkeys) {
  for (let round = 1; round <= 20; round += 1) {
    for (const monkey of monkeys) {
      const length = monkey.items.length
      for (let i = 0; i < length; i += 1) {
        const old = monkey.items.shift()
        monkey.inspections += 1
        const worrylevel = Math.floor(eval(monkey.operation) / 3) // eslint-disable-line 
        const nextMonkey = (worrylevel % monkey.test === 0) ? monkey.ifTrue : monkey.ifFalse
        monkeys[nextMonkey].items.push(worrylevel)
      }
    }
    // console.log(round, monkeys)
  }

  // always use own comparator, otherwise it is sorted alphabetically!!
  const [first, second] = monkeys.map(m => m.inspections).sort((a, b) => b - a)
  return first * second
}

function partTwo (monkeys) {
  return 'todo'
}
