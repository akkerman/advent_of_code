R = require('ramda')

const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let tally = [ 0,0,0,0,0,0,0,0,0,0,0,0 ]
let lines = []

const add = (a,b) => {
  return  Number.parseInt(a) + Number.parseInt(b)
}

rl.on("line", data => {
const input = data.split('')
  tally = R.zipWith(add, input, tally)
  lines.push(data)
})

rl.on('close', () => {
  const half = lines.length / 2
  console.log(tally)

  const rates = tally.reduce(({gamma, epsilon}, curr) => {
    if (curr > half) {
      return { gamma: `${gamma}1`, epsilon: `${epsilon}0` }
    } else {
      return { gamma: `${gamma}0`, epsilon: `${epsilon}1` }
    }

  }, {gamma:"", epsilon:""})

  const answer = { }
  answer.gamma = Number.parseInt(rates.gamma, 2)
  answer.epsilon = Number.parseInt(rates.epsilon, 2)
  answer.partOne = answer.gamma * answer.epsilon

  console.log({rates, answer})

  const max =  (numZeros, numOnes) => numZeros <= numOnes ? '1' : '0'
  const min =  (numZeros, numOnes) => numZeros <= numOnes ? '0' : '1'

  const oxygen = filterLines( max, lines.slice(0), 0)
  const co2 = filterLines(min, lines.slice(0), 0)

  console.log({
    oxygen,  // 10111, or 23 in decimal.
    co2,     // 01010, or 10 in decimal.
  }) 

  answer.oxygen = Number.parseInt(oxygen,2)
  answer.co2 = Number.parseInt(co2,2)
  answer.partTwo = answer.oxygen * answer.co2 // 230


  console.log({answer})

})

function filterLines(pred, lines, idx) {
  const numOnes = lines.reduce((acc, line) => (line.charAt(idx) === '1' ? acc+1 : acc), 0)
  const numZeros = lines.length - numOnes
  const bitToKeep = pred(numZeros, numOnes)
  const filtered = lines.filter(l => l.charAt(idx) == bitToKeep)

  if (filtered.length === 1) {
    return filtered[0]
  }
  return filterLines(pred, filtered, idx+1)
}
