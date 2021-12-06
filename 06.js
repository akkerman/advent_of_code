const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let input

rl.on("line", data => {
 input =  data.split(',').map(i=>Number.parseInt(i))

})

rl.on('close', () => {
  console.log('partOne', partOne(input))
  console.log('partTwo', partTwo(input))
})

function partOne(input, iter=80) {
  return calc(input, iter)

  function calc(state, iter) {
    if (iter === 0) return state.length

    const newFish = []
    for (let i=0; i<state.length; i+=1) {
      if (state[i] === 0) {
        newFish.push(8)
        state[i]=6
      } else {
        state[i]-=1
      }
    }
    return calc(state.concat(newFish), iter-1)
  }
}

function partTwo(lines) {
  return 'todo'
}
