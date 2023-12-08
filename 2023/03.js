const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = fn => args => { fn(args); return args } // eslint-disable-line 
const log = console.log // eslint-disable-line
const sum = (a, b) => a + b

const lines = []

rl.on('line', data => {
  const line = data

  lines.push(line)
})

let maxRow = 0
let maxCol = 0
rl.on('close', () => {
  maxRow = lines.length - 1
  maxCol = lines[0].length - 1
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const digits = '0123456789'

function partOne (lines) {
  const partNumbers = []
  for (let r = 0; r <= maxRow; r += 1) {
    for (let c = 0; c <= maxCol; c += 1) {
      const char = lines[r][c]
      if (digits.includes(char) || char === '.') {
        continue
      }

      for (let dr = r - 1; dr <= r + 1; dr += 1) {
        for (let dc = c - 1; dc <= c + 1; dc += 1) {
          if (dr < 0 || dc < 0 || dr > maxRow || dc > maxCol) { continue }
          if (digits.includes(lines[dr][dc])) {
            const [coord, partNr] = getPartnumberAt(dr, dc)
            partNumbers[coord] = partNr
          }
        }
      }
    }
  }
  Object.entries(partNumbers).forEach(s => console.log(s))
  return Object.values(partNumbers).reduce(sum)
}

function getPartnumberAt (r, c) {
  let start
  let partNr = ''

  for (let dc = c; dc >= 0; dc -= 1) {
    start = dc
    if (!digits.includes(lines[r][dc])) {
      start += 1
      break
    }
  }
  for (let dc = start; dc <= maxCol; dc += 1) {
    if (!digits.includes(lines[r][dc])) {
      break
    }
    partNr += lines[r][dc]
  }
  return [`${r + 1},${start + 1}`, parseInt(partNr)]
}

function partTwo (lines) {
  const gearRatios = []
  let partNumbers = {}
  for (let r = 0; r <= maxRow; r += 1) {
    for (let c = 0; c <= maxCol; c += 1) {
      const char = lines[r][c]
      if (char !== '*') {
        continue
      }

      for (let dr = r - 1; dr <= r + 1; dr += 1) {
        for (let dc = c - 1; dc <= c + 1; dc += 1) {
          if (dr < 0 || dc < 0 || dr > maxRow || dc > maxCol) { continue }
          if (digits.includes(lines[dr][dc])) {
            const [coord, partNr] = getPartnumberAt(dr, dc)
            partNumbers[coord] = partNr
          }
        }
      }
      const gears = Object.values(partNumbers)
      if (gears.length === 2) {
        gearRatios.push(gears[0] * gears[1])
      }
      partNumbers = {}
    }
  }
  return gearRatios.reduce(sum)
}
