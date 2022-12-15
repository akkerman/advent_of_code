const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

let minX = Infinity
let maxX = 0
let minY = Infinity
let maxY = 0

rl.on('line', data => {
  const [sx, sy, bx, by] = data
    .replace('Sensor at x=', '')
    .replace(' closest beacon is at x=', '')
    .replaceAll(' y=', '')
    .split(':')
    .flatMap(x => x.split(','))
    .map(y => parseInt(y))

  const distance = manhattanDistance([sx, sy], [bx, by])
  minX = Math.min(minX, sx - distance, bx - distance)
  maxX = Math.max(maxX, sx + distance, bx + distance)
  minY = Math.min(minY, sy - distance, by - distance)
  maxY = Math.max(maxY, sy + distance, by + distance)

  lines.push([sx, sy, bx, by, distance])
})

// x -> col
// y -> row
const manhattanDistance = ([x1, y1], [x2, y2]) =>
  Math.abs(x1 - x2) + Math.abs(y1 - y2)

/**
 * @param {Array<Integer>} param1
 * @param {Integer} d
 * @param {Integer} y
 */
const rangeFrom = ([x1, y1], d, y) => {
  const tmp = d - Math.abs(y1 - y)
  if (tmp < 0) return
  const a = x1 - tmp
  const b = x1 + tmp
  return [Math.max(minX, a), Math.min(maxX, b)]
}

rl.on('close', () => {
  // console.log({ minX, maxX, diff: maxX - minX, minY, maxY })
  // console.log('partOne - example', partOne(lines, 10))
  // console.log('partOne - input', partOne(lines, 2000000))
  console.log('partOne', partOneAlternative(lines, 2000000))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines, y) {
  const locations = new Set()
  for (let x = minX; x <= maxX; x += 1) {
    const label = `${x}_${y}`
    for (const [sx, sy,,, d] of lines) {
      if (manhattanDistance([sx, sy], [x, y]) <= d) {
        locations.add(label)
        break
      }
    }
  }

  const beaconsOnRow = new Set(
    lines
      .map(([,, bx, by]) => by === y ? `${bx}_${by}` : null)
      .filter(_ => _),
  )
  return locations.size - beaconsOnRow.size
}

function partOneAlternative (lines, y) {
  const segments = lines.map(
    ([sx, sy, bx, by, d]) => rangeFrom([sx, sy], d, y),
  ).filter(_ => _).sort(([a], [b]) => a - b)

  const first = segments.shift()
  const row = segments.reduce(
    (acc, curr) => {
      const end = acc[acc.length - 1]

      if (end >= curr[1]) return acc

      if (end >= curr[0]) {
        return acc.slice(0, -1).concat(curr[1])
      }
      return acc.concat(curr)
    },
    first)

  return (row[1] - row[0])
}

function partTwo (lines) {
  const max = 4000000
  maxX = max
  minX = 0
  let row = []
  let y = 0

  for (y = 0; y <= max; y += 1) {
    const segments = lines.map(
      ([sx, sy, bx, by, d]) => rangeFrom([sx, sy], d, y),
    ).filter(_ => _).sort(([a], [b]) => a - b)

    const first = segments.shift()

    row = segments.reduce(
      (acc, curr) => {
        const end = acc[acc.length - 1]

        if (end >= curr[1]) return acc

        if (end >= curr[0]) {
          return acc.slice(0, -1).concat(curr[1])
        }
        return acc.concat(curr)
      },
      first)

    if (row.length > 2) {
      break
    }
  }

  const [, preX] = row

  const x = preX + 1

  return max * x + y
}
