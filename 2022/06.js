const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

const R = require('ramda')

rl.on('line', data => {
  const line = data

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', lines.map(partOne)) // 7,5,6,10,11
  console.log('partTwo', partOne(lines[0]))
  console.log('partTwo', partTwo(lines[0]))
})

function partOne (stream) {
  let idx = 0
  for (let i = 0; i < stream.length - 4; i += 1) {
    const [w, x, y, z] = stream.slice(i, i + 4)
    console.log([w, x, y, z])
    if (
      ![x, y, z].includes(w) &&
      ![y, z].includes(x) &&
      y !== z
    ) {
      return idx + 4
    }
    idx += 1
  }
  return -1
}

function partTwo (stream) {
  return 'todo'
}
