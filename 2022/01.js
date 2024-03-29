const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data === '' ? '' : Number.parseInt(data)

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  const totals = lines.reduce((acc, cur) => {
    if (cur === '') {
      return {
        max: Math.max(acc.max, acc.elf),
        elf: 0,
      }
    } else {
      acc.elf += cur
      return acc
    }
  }, { max: 0, elf: 0 })

  return totals.max
}

function partTwo (lines) {
  const totals = lines.reduce((acc, cur) => {
    if (cur === '') {
      acc.push(0)
    } else {
      acc[acc.length - 1] += cur
    }
    return acc
  }, [0])

  return totals.sort((a, b) => b - a).slice(0, 3).reduce((a, b) => a + b)
}
