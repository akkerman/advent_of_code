const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const valves = new Map()

const re = /Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.+)/

rl.on('line', data => {
  const result = re.exec(data)
  if (!result) {
    console.error('No result for', data)
    return
  }

  const [, id, rate, to] = result
  valves.set(id, Object.freeze({
    id,
    rate: parseInt(rate),
    to: to.split(', '),
  }))
})

rl.on('close', () => {
  // prepare neigbours
  const neigbours = new Map()

  for (const [, valve] of valves) {
    neigbours.set(
      valve,
      valve.to.map(id => valves.get(id)).sort((a, b) => b.rate - a.rate),
    )
  }
  console.log(neigbours)

  console.log('partOne', partOne(valves))
  console.log('partTwo', partTwo(valves))
})

function partOne (cave) {
  // how many non zero valves are there? GO there asap

  return 'todo'
}

function partTwo (cave) {
  return 'todo'
}
