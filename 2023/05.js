const {
  Worker, isMainThread, parentPort, workerData,
} = require('node:worker_threads')

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
  partTwo(almanac)
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
  const workers = []
  const minima = []
  for (let idx = 0; idx < almanac.seeds.length; idx += 2) {
    const start = almanac.seeds[idx]
    const length = almanac.seeds[idx + 1]

    const worker = new Worker(
      './05-worker.js',
      { workerData: { maps: almanac.maps, start, length } },
    )
    workers.push(worker)
    worker.on('message', min => {
      console.log({ start, length, min })
      minima.push(min)
      if (minima.length === workers.length) {
        console.log('partTwo', Math.min(...minima))
      }
    })
  }
}
