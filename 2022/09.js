const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const [direction, steps] = data.split(' ')

  lines.push({ direction, steps: parseInt(steps) })
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  return solve(lines, 2)
}
function partTwo (lines) {
  return solve(lines, 10)
}
function solve (lines, length) {
  const rope = Array.from({ length }, () => [0, 0])

  const tailpositions = new Set()

  for (const { direction, steps } of lines) {
    for (let i = 0; i < steps; i += 1) {
      let head = rope[0]
      let tail = rope[1]
      switch (direction) {
        case 'U':
          head[1] -= 1
          break
        case 'D':
          head[1] += 1
          break
        case 'L':
          head[0] -= 1
          break
        case 'R':
          head[0] += 1
          break
        default:
        // noop
      }

      for (let knot = 0; knot < rope.length - 1; knot += 1) {
        head = rope[knot]
        tail = rope[knot + 1]

        if (tail[0] === head[0] && tail[1] < head[1] - 1) {
          tail[1] = head[1] - 1
        } else

        if (tail[0] === head[0] && tail[1] > head[1] + 1) {
          tail[1] = head[1] + 1
        } else

        if (tail[1] === head[1] && tail[0] < head[0] - 1) {
          tail[0] = head[0] - 1
        } else

        if (tail[1] === head[1] && tail[0] > head[0] + 1) {
          tail[0] = head[0] + 1
        } else

        if (tail[0] !== head[0] && tail[1] !== head[1]) {
          const lr = head[0] - tail[0]
          const ud = head[1] - tail[1]

          if (Math.abs(lr) > 1 || Math.abs(ud) > 1) {
            tail[0] += (lr > 0 ? 1 : -1)
            tail[1] += (ud > 0 ? 1 : -1)
          }
        }
      }

      tailpositions.add(tail.join(','))
    }
  }

  return tailpositions.size // 2527 is too high
}
