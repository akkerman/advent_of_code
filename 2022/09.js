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
  const head = [0, 0]
  const tail = [0, 0]

  const tailpositions = new Set()

  for (const { direction, steps } of lines) {
    for (let i = 0; i < steps; i += 1) {
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

      tailpositions.add(tail.join(','))
    }
  }

  return tailpositions.size
}

function partTwo (lines) {
  return 'todo'
}
