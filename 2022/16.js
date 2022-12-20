const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const valvesDict = {}
const valves = []

const re = /Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.+)/

rl.on('line', data => {
  const result = re.exec(data)
  if (!result) {
    console.error('No result for', data)
    return
  }

  const [, id, rate, to] = result
  const valve = Object.freeze({
    id,
    rate: parseInt(rate),
    neighbours: to.split(', '),
  })
  valvesDict[id] = valve
  valves.push(valve)
})

function shortestPathMatrix () {
  const start = valvesDict.AA
  const matrix = {}
  const activeValves = Object.values(valvesDict).filter(v => v.rate)

  return matrix
}

/**
 * Floyd-Warshall
 * `Algorithms in a Nutshell` p. 161
 */
function allPairsShortestPath (G) {
  const dist = {}
  const pred = {}

  for (const valve of G) {
    const u = valve.id
    dist[u] = { }
    pred[u] = {}
    for (const { id: v } of G) {
      dist[u][v] = Infinity
      pred[u][v] = null
    }
    dist[u][u] = 0
    for (const v of valve.neighbours) {
      dist[u][v] = 1
      pred[u][v] = u
    }
  }

  for (const { id: k } of G) {
    for (const { id: u } of G) {
      for (const { id: v } of G) {
        const newLen = dist[u][k] + dist[k][v]
        if (newLen < dist[u][v]) {
          dist[u][v] = newLen
          pred[u][v] = pred[k][v]
        }
      }
    }
  }
  return { dist, pred }
}

function purgeBrokenValves (matrix) {
  const brokenValves = valves.filter(v => v.id !== 'AA' && v.rate === 0).map(v => v.id)
  for (const { id } of valves) {
    if (brokenValves.includes(id)) {
      delete matrix[id]
      continue
    }
    for (const broken of brokenValves) {
      delete matrix[id][broken]
    }
  }
  return matrix
}

function dfsFlowRate (dist, time, valve) {
  const cache = new Map()
  return dfsFlowRatePrime(time, valve, 0, [valve])

  function dfsFlowRatePrime (time, valve, pressure, on) {
    if (time <= 0) return pressure
    const key = JSON.stringify({ time, valve, on })
    if (cache.has(key)) return cache.get(key)

    let newPressure = pressure
    for (const [id, mins] of Object.entries(dist[valve])) {
      if (on.includes(id)) continue

      const newTime = time - mins - 1
      const p = dfsFlowRatePrime(newTime, id, pressure + newTime * valvesDict[id].rate, [...on, id])
      cache.set(key, p)
      newPressure = Math.max(newPressure, p)
    }
    return newPressure
  }
}

rl.on('close', () => {
  console.log('partOne', partOne())
  console.log('partTwo', partTwo())
})

function partOne () {
  const dist = purgeBrokenValves(allPairsShortestPath(valves).dist)

  const flow = dfsFlowRate(dist, 30, 'AA')
  return flow
}

function partTwo (cave) {
  return 'todo'
}
