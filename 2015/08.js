const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data
  
  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  for (const line of lines) {
    console.log(line, JSON.stringify(line))
  }
  return 'todo'
}

function partTwo (lines) {
  return 'todo'
}
