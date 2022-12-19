const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data
    .replaceAll(/[A-Za-z:.]/g, '')
    .trim()
    .replaceAll(/ +/g, ',')

  lines.push(JSON.parse(`[${line}]`))
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function bluePrint ([id, ore, clay, obsOre, obsClay, geoOre, geoObs]) {
  return {
    id,
    robots: [
      { type: 'ore', ore, clay: 0, obsidian: 0 },
      { type: 'clay', ore: 0, clay, obsidian: 0 },
      { type: 'obsidian', ore: obsOre, clay: obsClay, obsidian: 0 },
      { type: 'geode', ore: geoOre, clay: 0, obsidian: geoObs },
    ],
  }
}

function partOne (lines) {
  return 'todo'
}

function partTwo (lines) {
  return 'todo'
}
