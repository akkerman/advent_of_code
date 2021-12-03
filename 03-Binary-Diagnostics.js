R = require('ramda')

const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let tally = [ 0,0,0,0,0,0,0,0,0,0,0,0 ]
let lines = 0

const add = (a,b) => {
return  Number.parseInt(a) + Number.parseInt(b)
}

rl.on("line", data => {
const  input = data.split('')
  tally = R.zipWith(add, input, tally)
  lines += 1
})

rl.on('close', () => {
  const half = lines /2
  console.log(tally)

  const rates = tally.reduce(({gamma, epsilon}, curr) => {
    if (curr > half) {
      return { gamma: `${gamma}1`, epsilon: `${epsilon}0` }
    } else {
      return { gamma: `${gamma}0`, epsilon: `${epsilon}1` }
    }

  }, {gamma:"", epsilon:""})

  console.log(rates)

  console.log(
    Number.parseInt(rates.gamma, 2) *
    Number.parseInt(rates.epsilon, 2)
  )

})
