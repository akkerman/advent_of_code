
// target area: x=265..287, y=-103..-58
const input = [256, 287, -103, -58] // input
const example = [20, 30, -10, -5] // example

function createTrajectoryCalculator (minX, maxX, minY, maxY) {
  function isHit (x, y) {
    return (
      minX <= x && x <= maxX &&
    minY <= y && y <= maxY
    )
  }

  function isOut (x, y) {
    return (
      maxX < x || y < minY
    )
  }

  function step (x, y, vx, vy) {
    return [
      x + vx,
      y + vy,
      Math.max(0, vx - 1),
      vy - 1
    ]
  }

  return function calculate (vx, vy) {
    let x = 0
    let y = 0
    let maxHeight = 0
    while (true) {
      const [x1, y1, vx1, vy1] = step(x, y, vx, vy)

      if (isOut(x1, y1)) {
        return [false, 0]
      }

      maxHeight = Math.max(maxHeight, y1)

      if (isHit(x1, y1)) {
        return [true, maxHeight]
      }
      [x, y, vx, vy] = [x1, y1, vx1, vy1]
    }
  }
}

function createVelocityGenerator (_, maxX) {
  return function * generate () {
    for (let vx = 1; vx < maxX; vx += 1) {
      for (let vy = 1; vy < 1000; vy += 1) {
        yield [vx, vy]
      }
    }
  }
}

const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data

  lines.push(line)
})

function partOne () {
  const calc = createTrajectoryCalculator(...input)
  const velo = createVelocityGenerator(...input)

  let max = 0
  for (const v of velo()) {
    const [hit, height] = calc(...v)
    if (hit) {
      max = Math.max(max, height)
    }
  }

  return max
}

function partTwo () {
  return 'todo'
}

function main () {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
}

main()
