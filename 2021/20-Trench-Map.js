const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const trenchmap = []
let enhancementAlgorithm

rl.on('line', data => {
  if (data === '') return
  if (!enhancementAlgorithm) {
    enhancementAlgorithm = data
    return
  }

  const line = data.split('')

  trenchmap.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(trenchmap, enhancementAlgorithm))
  console.log('partTwo', partTwo(trenchmap, enhancementAlgorithm))
})

const int = i => Number.parseInt(i, 2)

function pp (trenchmap) { // eslint-disable-line 
  for (const trench of trenchmap) {
    console.log(trench.join(''))
  }
}

const makeGetNeighbours = (trenchmap, filler) => (row, col) => {
  const height = trenchmap.length
  const width = trenchmap[0].length
  const neighbours = []
  for (let r = row - 1; r <= row + 1; r += 1) {
    for (let c = col - 1; c <= col + 1; c += 1) {
      if (r < 0 || c < 0 || width <= c || height <= r) {
        neighbours.push(filler)
      } else {
        neighbours.push(trenchmap[r][c])
      }
    }
  }
  return neighbours
}

const neighbours2number = neighbours => int(neighbours.map(c => c === '#' ? 1 : 0).join(''))

const createEmpty = (trenchmap) => {
  const width = trenchmap[0].length
  const height = trenchmap.length
  return Array.from({ length: height }, () => Array.from({ length: width }).fill('.'))
}

const enlarge = (trenchmap, filler) => {
  const width = trenchmap[0].length + 2
  const enlarged = trenchmap.map(trench => [filler, ...trench, filler])
  enlarged.push(Array(width).fill(filler))
  enlarged.unshift(Array(width).fill(filler))
  return enlarged
}

function * generateCoordinates (trenchmap) {
  for (let row = 0; row < trenchmap.length; row += 1) {
    for (let col = 0; col < trenchmap[0].length; col += 1) {
      yield [row, col]
    }
  }
}

const getEnhancement = num => enhancementAlgorithm.charAt(num)

function enhance (trenchmap, filler) {
  const getNeighbours = makeGetNeighbours(trenchmap, filler)
  const enhancedTrenchMap = createEmpty(trenchmap)

  for (const [row, col] of generateCoordinates(trenchmap)) {
    const neighbours = getNeighbours(row, col)
    const enhancement = getEnhancement(neighbours2number(neighbours))
    enhancedTrenchMap[row][col] = enhancement
  }

  return enhancedTrenchMap
}

// The absolute trick for this puzzle is knowing how to handle the infinite part.
//
// Before starting enhancing the image, the known universe, outside the image,
// is '.' (dark) this translates to all zeroes, thus after the first step
// the whole universe is enhanced to the first entry from the enhancementAlgorithm
//
// in the example input this is still dark, so nothing ever changes outside the image
//
// in the actual input this is '#' (light) effectivly changing the whole
// universe to light. Everything being light translates to 111111111 in binary,
// 511 decimal so the last entry form the enhancementAlgorithm will be used.
//
// In the input this last entry is '.' (dark), this means that everything
// outside the original image switches on and off with each step.
//
// Consider an alternative input where the first and last enhancement are both light '#',
// the known universe would switch to light after the first step and stay that way.
function determineFiller (step) {
  const first = getEnhancement(0)
  const last = getEnhancement(511)
  if (step === 0 || first === '.') return '.'
  if (first === last) return first // both '#'
  return step % 2 === 0 ? last : first // switch on/off
}

function solve (steps) {
  let enhanced = trenchmap
  for (let step = 0; step < steps; step++) {
    const filler = determineFiller(step)
    const larger = enlarge(enhanced, filler)
    enhanced = enhance(larger, filler)
  }

  return enhanced
}

const count = trenchmap => trenchmap
  .flatMap(trench => trench.filter(e => e === '#').length)
  .reduce((a, b) => a + b)

function partOne () {
  return count(solve(2)) // 35 // 5361
}

function partTwo () {
  return count(solve(50)) // 3351 // 16826
}
