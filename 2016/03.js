const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data.trim().replace(/ +/g, ' ').split(' ').map(i => parseInt(i))
  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  let sum=0
  for (line of lines){
    const [a,b,c]=   line.sort((a,b)=>a-b)
    if (a+b > c) sum+=1
  }
  return sum
}

function partTwo (lines) {
  return 'todo'
}
