const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let input

rl.on("line", data => {
 input =  data.split(',').map(i=>Number.parseInt(i))

})

rl.on('close', () => {
  console.log('partOne', solve(input, 80))  // 5934        // 387413
  console.log('partTwo', solve(input, 256)) // 26984457539 // 1738377086345
})


function solve(input, days=10) {
  let state = input.reduce(
    (state, i) => {state[i]++; return state},
  Array(9).fill(0))


  for (let d=0; d<days; d+=1) {
    const [breeders, ...rest] = state
    state = [...rest, breeders]
    state[6]+=breeders
  }

  return state.reduce((a,b)=>a+b)
} 

