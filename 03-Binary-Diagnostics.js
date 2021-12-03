const { add, filter, gt, join, lte, map, pipe, prop, propEq, reduce, split, subtract, sum, zipWith, } = require('ramda')

const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const lines = []

rl.on("line", data => {
  const input = pipe(split(''), map(s=>Number.parseInt(s)))(data)

  lines.push(input)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const toi = pipe(join(''), s => Number.parseInt(s, 2))

function partOne(lines) {
  const tally = reduce(zipWith(add), Array(12).fill(0))(lines)
  const half = lines.length / 2
  const gamma = map(i => +(half < i), tally)
  const epsilon = zipWith(subtract, Array(12).fill(1), gamma)
  return toi(gamma) * toi(epsilon) // 198
}

function partTwo(lines) {
  const oxygen = filterLines(lte, lines)
  const co2 = filterLines(gt, lines)
  return  toi(oxygen) * toi(co2) // 230
}

function filterLines(pred, lines) {
  return solve(lines)
  function solve (lines, idx=0) {
    const numOnes = pipe(map(prop(idx)), sum)(lines)
    const numZeroes = lines.length - numOnes
    const filtered = filter(propEq(idx, +(pred(numZeroes, numOnes))))(lines)
    if (filtered.length === 1) return filtered[0]
    return solve(filtered, idx+1)
  }
}
