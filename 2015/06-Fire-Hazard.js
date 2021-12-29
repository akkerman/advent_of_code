const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const instructions = []

const ints = str => str.split(',').map(Number)

rl.on("line", data => {
  const [operation, from, to] = data.replace("turn ", "").replace("through ", "").split(" ")


  instructions.push([operation, ints(from), ints(to)])
})


const englishOperations = {
  on: x => 1,
  off: x => 0,
  toggle: x => x === 1 ? 0 : 1
}

const elfOperations = {
  on: x => x+1,
  off: x => Math.max(0, x-1),
  toggle: x => x + 2
}

function * coordinates ([x1,y1], [x2,y2]) {
  for (let x=x1; x<=x2; x+=1)
  for (let y=y1; y<=y2; y+=1)
  yield [x,y]
}

function solve (operations) {
  const lights = new Uint8Array(1000*1000)

  for (const [op, from, to] of instructions)
  for (let [x,y] of coordinates(from, to)) {
    const pos = 1000*x + y
    lights[pos] = operations[op](lights[pos])
  }
  return lights.reduce((a,b)=>a+b)
}

rl.on('close', () => {
  console.log('partOne', solve(englishOperations))
  console.log('partTwo', solve(elfOperations))
})
