const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = fn => args => { fn(args); return args } // eslint-disable-line 
const log = console.log // eslint-disable-line
const sum = (a, b) => a + b // eslint-disable-line 

const toInt = s => parseInt(s.trim())

let currentMap

const almanac = {
  seeds: undefined,
  maps: [],
}

rl.on('line', data => {
  if (data === '') return
  if (!almanac.seeds) {
    almanac.seeds = data.split(': ')[1].split(' ').map(toInt)
    return
  }

  if (data.includes('map')) {
    currentMap = []
    almanac.maps.push(currentMap)
    return
  }

  currentMap.push(data.split(' ').map(toInt))
})

rl.on('close', () => {
  console.log('partOne', partOne(almanac))
  console.log('partTwo', partTwo(almanac))
})

function makeCategoryMap (map) {
  return source => {
    for (const [dest, start, length] of map) {
      if (start <= source && source < start + length) {
        return dest - start + source
      }
    }
    return source
  }
}

function partOne (almanac) {
  const fns = almanac.maps.map(makeCategoryMap)
  const seedLocations = almanac.seeds.map(R.pipe(...fns))

  return Math.min(...seedLocations)
}

function partTwo (almanac) {
  return 'todo'
}
