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

function getLimitsAroundRock (lines) {
  return lines.reduce((
    { minX, minY, minZ, maxX, maxY, maxZ },
    [x, y, z]) => {
    return {
      minX: Math.min(minX, x - 1),
      minY: Math.min(minY, y - 1),
      minZ: Math.min(minZ, z - 1),
      maxX: Math.max(maxX, x + 1),
      maxY: Math.max(maxY, y + 1),
      maxZ: Math.max(maxZ, z + 1),
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
  // oposide points of a cube around the rock
  const { minX, minY, minZ, maxX, maxY, maxZ } = getLimitsAroundRock(lines)
  const airLabels = new Set()
  const air = []
  console.log({ minX, minY, minZ, maxX, maxY, maxZ })

  const isOutside = ([x, y, z]) => {
    return (
      x < minX || y < minY || z < minZ ||
      x > maxX || y > maxY || z > maxZ
    )
  }

  const queue = [[minX, minY, minZ]]
  airLabels.add(label([minX, minY, minZ]))

  // all 1x1x1 cubs of air around the rock
  while (queue.length) {
    const [x, y, z] = queue.shift()
    for (const [dx, dy, dz] of directions) {
      const neighbour = [x + dx, y + dy, z + dz]
      if (isOutside(neighbour)) continue
      const nbl = label(neighbour)
      if (cubes.has(nbl) || airLabels.has(nbl)) {
        continue
      }
      airLabels.add(nbl)
      air.push(neighbour)

      queue.push(neighbour)
    }
  }

  // figure out if stuff is touching
  let area = 0
  for (const [x, y, z] of air) {
    for (const [dx, dy, dz] of directions) {
      const neighbour = [x + dx, y + dy, z + dz]
      if (cubes.has(label(neighbour))) {
        area += 1
      }
    }
  }
  return area
}
