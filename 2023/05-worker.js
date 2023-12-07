const R = require('ramda')
const {
  isMainThread, parentPort, workerData,
} = require('node:worker_threads')

if (isMainThread) {
  console.error('can only run as worker')
  process.exit(1)
}

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

const {
  maps,
  start,
  length,
} = workerData

const seedToLocation = R.pipe(...maps.map(makeCategoryMap))

let min = Number.MAX_SAFE_INTEGER
for (let seed = start; seed < start + length; seed += 1) {
  min = Math.min(min, seedToLocation(seed))
}

parentPort.postMessage(min)
