const {zipWith,reduce,add,pipe,map} = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let input

rl.on("line", data => {
  input = data.split(',').map(i => Number.parseInt(i))
})


const sub = (a,b) => Math.abs(a-b)
const sum = reduce(add,0)
const subtract = zipWith(sub)
const fill = (length,num) => Array(length).fill(num)
const stepToFuel = n => n * (n + 1) / 2
const incfuel = pipe(subtract, map(stepToFuel))

function solve(input, fuelcalculator) {
  const min = Math.min(...input)
  const max = Math.max(...input)
  const len = input.length
  let hor = 0
  let fuel = Number.MAX_SAFE_INTEGER

  for (let i=min; i<=max; i+=1) {
    const cost = sum(fuelcalculator(input, fill(len, i)))
    if (cost < fuel) {
      fuel = cost
      hor = i
    }
  }
  return {hor, fuel}
}

rl.on('close', () => {
  console.log('partOne', solve(input, subtract), "341558")
  console.log('partTwo', solve(input, incfuel), "93214037")
})
