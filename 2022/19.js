const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data
    .replaceAll(/[A-Za-z:.]/g, '')
    .trim()
    .replaceAll(/ +/g, ',')

  lines.push(bluePrint(JSON.parse(`[${line}]`)))
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const resourceTypes = ['obsidian', 'ore', 'clay']

function bluePrint ([id, oreOre, clayOre, obsOre, obsClay, geoOre, geoObs]) {
  return {
    id,
    robots: [
      { type: 'ore', ore: oreOre, clay: 0, obsidian: 0 },
      { type: 'clay', ore: clayOre, clay: 0, obsidian: 0 },
      { type: 'obsidian', ore: obsOre, clay: obsClay, obsidian: 0 },
      { type: 'geode', ore: geoOre, clay: 0, obsidian: geoObs },
    ],
  }
}

function maxGeodes (blueprint) {
  const initialRobots = { ore: 1, clay: 0, obsidian: 0, geode: 0 }
  const initialResources = { ore: 0, clay: 0, obsidian: 0, geode: 0 }
  const maxSpend = blueprint.robots.reduce((cost, robot) => {
    for (const type of resourceTypes) {
      cost[type] = Math.max(cost[type] || 0, robot[type] || 0)
    }
    return cost
  }, {
  })

  const cache = new Map()

  return dfs(24, initialRobots, initialResources)

  function dfs (time, bots, resources) {
    const key = JSON.stringify({ time, bots, resources })
    if (cache.has(key)) {
      return cache.get(key)
    }
    if (time <= 0) {
      return resources.geode
    }

    // console.log(time, bots, resources)

    let maxGeodes = bots.geode * time + resources.geode

    for (const currentBot of blueprint.robots) {
      const newBots = { ...bots }
      const newResources = { ...resources }
      for (const type of ['geode', ...resourceTypes]) {
        newResources[type] += bots[type]
      }

      // do we have the resources to build this bot
      const canBuildBot = resourceTypes.reduce((canBuild, type) => canBuild && currentBot[type] <= resources[type], true)
      // we always want a geode bot, but we do not need to gather more resources in a minute than we can spend in a minute
      const wantBuildBot = currentBot.type === 'geode' || bots[currentBot.type] < maxSpend[currentBot.type]

      if (wantBuildBot && canBuildBot) {
        for (const type of resourceTypes) {
          newResources[type] -= currentBot[type]
        }
        newBots[currentBot.type] += 1
      }
      const result = dfs(time - 1, newBots, newResources)
      cache.set(key, result)

      maxGeodes = Math.max(maxGeodes, result)
    }

    return maxGeodes
  }
}

function partOne (blueprints) {
  const mx = blueprints.map(maxGeodes)
  console.log(mx)
  return mx.map((m, i) => (i + 1) * m).reduce((a, b) => a + b)
}

function partTwo (lines) {
  return 'todo'
}
