const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

// x -> right -> column
// y -> down -> row
// for placing sand there is no difference between sand and path

let minX = Infinity
let maxX = 0
let minY = Infinity
let maxY = 0

rl.on('line', data => {
  const pairs = data.split(' -> ').map(p => {
    const [x, y] = p.split(',').map(i => parseInt(i))

    minX = Math.min(minX, x)
    maxX = Math.max(maxX, x)
    minY = Math.min(minY, y)
    maxY = Math.max(maxY, y)

    return [x, y]
  })

  lines.push(pairs)
})

rl.on('close', () => {
  console.log({ minX, maxX, minY, maxY })
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const label = (x, y) => `${x}_${y}`
function createReservoirWithPaths (lines) {
  const reservoir = new Set()
  for (const line of lines) {
    for (let i = 0; i < line.length - 1; i++) {
      const [fromX, fromY] = line[i]
      const [toX, toY] = line[i + 1]

      for (let x = Math.min(fromX, toX); x <= Math.max(fromX, toX); x += 1) {
        for (let y = Math.min(fromY, toY); y <= Math.max(fromY, toY); y += 1) {
          reservoir.add(label(x, y))
        }
      }
    }
  }
  return reservoir
}

function partOne (lines) {
  const reservoir = createReservoirWithPaths(lines)
  // start dumping sand
  let iterations = 0
  let units = 0
  while (true) {
    let x = 500
    let y = 0

    for (; y <= maxY + 1; y += 1) {
      if (!reservoir.has(label(x, y))) continue
      if (!reservoir.has(label(x - 1, y))) { x -= 1; continue }
      if (!reservoir.has(label(x + 1, y))) { x += 1; continue }
      reservoir.add(label(x, y - 1))
      units += 1
      break
    }
    if (y > maxY) {
      return units
    }

    if (iterations++ > 100000) {
      throw new Error('Infinity protection')
    }
  }
}

function partTwo (lines) {
  const reservoir = createReservoirWithPaths(lines)
  // start dumping sand
  let iterations = 0
  let units = 0

  const has = (x, y) => {
    return reservoir.has(label(x, y)) || y === maxY + 2
  }

  const startLabel = label(500, 0)
  while (true) {
    let x = 500
    let y = 0

    for (; y <= maxY + 2; y += 1) {
      if (!has(x, y)) continue
      if (!has(x - 1, y)) { x -= 1; continue }
      if (!has(x + 1, y)) { x += 1; continue }
      reservoir.add(label(x, y - 1))
      units += 1
      break
    }

    if (reservoir.has(startLabel)) {
      return units
    }

    if (iterations++ > 100000) {
      console.log(units)
      throw new Error('Infinity protection')
    }
  }
}
