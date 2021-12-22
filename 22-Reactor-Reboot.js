require('util').inspect.defaultOptions.depth = null
const { zipWith } = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

const parse = xs => xs.replace(/[xyz]=/, '').split('..').map(Number)
rl.on('line', data => {
  const [state, rest] = data.split(' ')
  const [x, y, z] = rest.split(',').map(parse).map(([min, max]) => [min, max + 1])

  lines.push([state, x, y, z])
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function * coords (x, y, z) {
  for (let xi = x[0]; xi < x[1]; xi += 1) {
    for (let yi = y[0]; yi < y[1]; yi += 1) {
      for (let zi = z[0]; zi < z[1]; zi += 1) {
        yield [xi, yi, zi]
      }
    }
  }
}

function intersectLine ([a, b], [x, y]) {
  const from = Math.max(a, x)
  const to = Math.min(b, y)
  return from < to && [from, to]
}

function intersectCube (cube1, cube2) {
  const intersection = zipWith(intersectLine, cube1, cube2)
  return intersection.every(_ => _) && intersection
}

const lineLength = ([a, b]) => (b - a)
const volume = cube => cube.map(lineLength).reduce((a, b) => a * b, 1)

function partOne (lines) {
  const cubes = new Set()
  for (const line of lines) {
    if (line.slice(1).flatMap(_ => _).some(i => i < -50 || i > 50)) continue

    const state = line[0]
    for (const coord of coords(...line.slice(1))) {
      const str = JSON.stringify(coord)
      if (state === 'on') {
        cubes.add(str)
      } else {
        cubes.delete(str)
      }
    }
  }
  return cubes.size
}

function partTwo (lines) {
  let cubes = []

  for (const line of lines) {
    const [state, ...newCube] = line

    // if (newCube.flatMap(_ => _).some(i => i < -50 || i > 50)) continue

    const intersections = cubes.map(([state, cube]) => [-state, intersectCube(cube, newCube)]).filter(([, cube]) => cube)
    cubes = cubes.concat(intersections)
    if (state === 'on') {
      cubes.push([1, newCube])
    }
  }
  return cubes.map(([state, cube]) => state * volume(cube)).reduce((a, b) => a + b, 0)
}

// NOTES
// Part one gave a huge oh oh, feeling
// pretty fast to implement
//
// First idea for part two was
// - keep a list of kubes that are on
// - if a new cube was to be added
//    - split the existing cubes and remove the intersection
//    - add new cube if on
//
// For some reason I could not get my head around splitting stuff in 3d.
// Worst case scenario was that it resulted in 27 new small cubes.
//
// Second idea was to keep two lists a 'on' list and an 'off' list.
// If an 'on' cube comes in add it to the 'on' list,
//    but also add it's intersection with the rest in the off list
// If an 'off' cube comes in only add the intersections to the off list
//
// Subtract the total volume of 'off' from the total volume of 'on'
// This does not work because ordering is important.
//
// Current Idea is to keep one ordered list with a number indicating 'on' and 'off' (1 and -1)
// If a cube comes in add all intersections with existing cubes with the opposite number.
// If the cube is on add it to the list.
//
// e.g. adding two 'on' cubes results in three cubes, the original with 'on' and the interseaction becomes 'off'
