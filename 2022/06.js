const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', lines.map(partOne)) // 7,5,6,10,11
  console.log('partTwo', lines.map(partTwo)) // 19,23,23,29,26
})

function partOne (stream) {
  let idx = 0
  for (let i = 0; i < stream.length - 4; i += 1) {
    const [w, x, y, z] = stream.slice(i, i + 4)
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
  let idx = 0
  for (let i = 0; i < stream.length - 4; i += 1) {
    const list = stream.slice(i, i + 14).split('')
    const set = new Set(list)
    if (set.size === list.length) {
      return idx + 14
    }
    idx += 1
  }
  return -1
}
