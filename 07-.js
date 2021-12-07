const {zipWith} = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let input

rl.on("line", data => {
  input = data.split(',').map(i => Number.parseInt(i))
})

rl.on('close', () => {
  console.log('partOne', partOne(input))
  console.log('partTwo', partTwo(input))
})

const sub = (a,b) => Math.abs(a-b)
const sum = input => input.reduce((a,b)=>a+b)
const subtract = (arr1, arr2) => zipWith(sub, arr1, arr2)
const fill = (length,num) => Array(length).fill(num)

function partOne(input) {
  const min = Math.min(...input)
  const max = Math.max(...input)
  const len = input.length
  let hor = 0
  let fuel = Number.MAX_SAFE_INTEGER
  

  for (let i=min; i<=max; i+=1) {
    const cost = sum(subtract(input, fill(len, i)))
    if (cost < fuel) {
      fuel = cost
      hor = i
    }
  }
  return {hor, fuel}
}

function partTwo(lines) {
  return 'todo'
}
