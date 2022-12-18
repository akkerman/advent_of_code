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

function getLimits (lines) {
  return lines.reduce((
    { minX, minY, minZ, maxX, maxY, maxZ },
    [x, y, z]) => {
    return {
      minX: Math.min(minX, x),
      minY: Math.min(minY, y),
      minZ: Math.min(minZ, z),
      maxX: Math.max(maxX, x),
      maxY: Math.max(maxY, y),
      maxZ: Math.max(maxZ, z),
    }
  },
  {
    minX: Infinity,
    minY: Infinity,
    minZ: Infinity,
    maxX: -Infinity,
    maxY: -Infinity,
    maxZ: -Infinity,
  })
}
function partTwo (lines) {
  // only calculates air pockets of exactly one within solid rock
  // not larger pockets IN rock, which works for example, not input
  const { minX, minY, minZ, maxX, maxY, maxZ } = getLimits(lines)
  let area = partOne(lines)

  for (let x = minX; x <= maxX; x += 1) {
    for (let y = minY; y <= maxY; y += 1) {
      for (let z = minZ; z <= maxZ; z += 1) {
        if (cubes.has(label([x, y, z]))) { // solid rock
          continue
        } // else air/water
        let air = true
        for (const [dx, dy, dz] of directions) {
          const neighbour = [x + dx, y + dy, z + dz]
          if (!cubes.has(label(neighbour))) {
            air = false
            break
          }
        }
        if (air) area -= 6
      }
    }
  }
  return area
  // too high: 3220
}
