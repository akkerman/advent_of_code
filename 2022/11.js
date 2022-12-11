const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const R = require('ramda')

let lines = []
const monkeys = []

function parseMonkey (lines) {
  let idx = 1
  return {
    items: lines[idx++].split(': ')[1].split(', ').map(i => parseInt(i)),
    operation: lines[idx++].split('new = ')[1],
    test: parseInt(lines[idx++].split('by ')[1]),
    true: parseInt(lines[idx++].split('monkey ')[1]),
    false: parseInt(lines[idx++].split('monkey ')[1]),
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
  console.log('partOne', solve(R.clone(monkeys), 20, 3))
  console.log('partTwo', solve(R.clone(monkeys), 10000, 1))
})

// zou kleinste gemene meervoud ook werken? hoe bepaal ik dat?
const lcm = R.compose(R.product, R.uniq, R.map(R.prop('test')))

function solve (monkeys, rounds, div) {
  const MOD = lcm(monkeys)

  for (let round = 1; round <= rounds; round += 1) {
    for (const monkey of monkeys) {
      const length = monkey.items.length
      monkey.inspections += length

      for (let i = 0; i < length; i += 1) {
        const old = monkey.items.shift()

        let worrylevel = Math.floor(eval(monkey.operation) / div)
        worrylevel = worrylevel % MOD

        const nextMonkey = monkey[worrylevel % monkey.test === 0]
        monkeys[nextMonkey].items.push(worrylevel)
      }
    }
  }

  // always use own comparator, otherwise it is sorted alphabetically!!
  const [first, second] = monkeys.map(m => m.inspections).sort((a, b) => b - a)
  return first * second
}
