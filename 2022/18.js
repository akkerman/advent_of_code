const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []
const cubes = new Set()

rl.on('line', data => {
  const line = data.split(',').map(i => parseInt(i))

  lines.push(line)
  cubes.add(label(line))
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const directions = [[1, 0, 0], [0, 1, 0], [0, 0, 1], [-1, 0, 0], [0, -1, 0], [0, 0, -1]]
function label ([x, y, z]) {
  return `${x},${y},${z}`
}

function partOne (lines) {
  let area = 0
  for (const [x, y, z] of lines) {
    for (const [dx, dy, dz] of directions) {
      const neighbour = [x + dx, y + dy, z + dz]
      if (!cubes.has(label(neighbour))) {
        area += 1
      }
    }
  }
  return area
}

function partTwo (lines) {
  return 'todo'
}
