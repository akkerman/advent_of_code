const R = require('ramda')
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
  const maxForOpenValves = new Map()

  const result = dfsFlowRatePrime(time, valve, 0, [valve])
  return [...result, maxForOpenValves]

  function record (pressure, on) {
    const key = JSON.stringify(on.sort())
    if (maxForOpenValves.has(key) && pressure < maxForOpenValves.get(key)) {
      return
    }
    maxForOpenValves.set(key, pressure)
  }

  function dfsFlowRatePrime (time, valve, pressure, on) {
    record(pressure, on)
    if (time <= 0) {
      return [pressure, on]
    }
    const key = JSON.stringify({ time, valve, on })
    if (cache.has(key)) return cache.get(key)

    let newPressure = pressure
    let newOn = on
    for (const [id, mins] of Object.entries(dist[valve])) {
      if (on.includes(id)) continue

      const newTime = time - mins - 1
      const [p, o] = dfsFlowRatePrime(newTime, id, pressure + newTime * valvesDict[id].rate, [...on, id])
      cache.set(key, [p, o])
      if (newPressure < p) {
        newPressure = p
        newOn = o
      }
    }

    return [newPressure, newOn]
  }
}

rl.on('close', () => {
  console.log('partOne', partOne())
  console.log('partTwo', partTwo())
})

function partOne () {
  const dist = purgeBrokenValves(allPairsShortestPath(valves).dist)

  const [flow] = dfsFlowRate(dist, 30, 'AA')
  return flow
}

function partTwo (cave) {
  const dist = purgeBrokenValves(allPairsShortestPath(valves).dist)

  const [,, f] = dfsFlowRate(dist, 26, 'AA')

  const flows = []
  for (const [key, value] of f.entries()) {
    flows.push([JSON.parse(key), value])
  }

  let maxFlowRate = 0
  while (flows.length > 0) {
    const [set1, flowRate1] = flows.pop()
    for (const [set2, flowRate2] of flows) {
      if (R.intersection(set1, set2).length === 1) {
        const flowRate = flowRate1 + flowRate2
        if (maxFlowRate < flowRate) {
          maxFlowRate = flowRate
        }
      }
    }
  }

  return maxFlowRate
}
