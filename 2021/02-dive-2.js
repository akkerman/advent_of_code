const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let f=0
let d=0
let a=0

rl.on("line", data => {
  const [movement, X] = data.split(' ')
  const x = Number.parseInt(X)

  switch (movement) {
    case 'down':
      a = a + x
      break
    case 'up':
      a = a - x
      break
    case 'forward':
      f = f + x
      d = d + (a * x)
      break
  }
})

rl.on('close', () => {
  console.log(f*d)
})
