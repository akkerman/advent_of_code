const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data.split('').map(i => parseInt(i))

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  let visible = 0
  const isSmaller = max => height => height < max

  const getLeft = (i, j) => lines[i].slice(0, j)
  const getRight = (i, j) => lines[i].slice(j + 1)
  const getUp = (i, j) => {
    const column = []
    for (let p = 0; p < i; p += 1) {
      column.push(lines[p][j])
    }
    return column
  }
  const getDown = (i, j) => {
    const column = []
    for (let p = i + 1; p < lines.length; p += 1) {
      column.push(lines[p][j])
    }
    return column
  }

  for (let i = 0; i < lines.length; i += 1) {
    for (let j = 0; j < lines[0].length; j += 1) {
      const max = lines[i][j]

      const isSmaller = height => height < max
      const left = getLeft(i, j)
      const right = getRight(i, j)
      const up = getUp(i, j)
      const down = getDown(i, j)

      if (!left.length || !right.length || !up.length || !down.length) {
        visible += 1
        continue
      }

      if (
        left.every(isSmaller) ||
        right.every(isSmaller) ||
        up.every(isSmaller) ||
        down.every(isSmaller)
      ) {
        visible += 1
      }
    }
  }
  return visible
}

function partTwo (lines) {
  return 'todo'
}
