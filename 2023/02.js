const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = fn => args => { fn(args); return args } // eslint-disable-line 
const log = console.log // eslint-disable-line

const games = [null]

rl.on('line', data => {
  const line = data

  const [, setStr] = data.split(':')
  const sets = setStr.split(';')
    .map(set =>
      set.split(',').map(s => s.trim()).map(s => s.split(' '))
        .reduce((s, [n, c]) => ({ ...s, [c]: parseInt(n) }), {}),
    )

  games.push(sets)
})

rl.on('close', () => {
  log('partOne', partOne(games))
  log('partTwo', partTwo(games))
})

function partOne (games) {
  let sum = 0
  const bag = { red: 12, green: 13, blue: 14 }

  for (let gameId = 1; gameId < games.length; gameId += 1) {
    const sets = games[gameId]
    let possible = true
    for (const set of sets) {
      if (set.red && set.red > bag.red) { possible = false; break }
      if (set.green && set.green > bag.green) { possible = false; break }
      if (set.blue && set.blue > bag.blue) { possible = false; break }
    }
    if (!possible) continue

    sum += gameId
  }

  return sum
}

function partTwo (games) {
  let sum = 0

  for (let gameId = 1; gameId < games.length; gameId += 1) {
    const bag = { red: 0, green: 0, blue: 0 }
    const sets = games[gameId]
    for (const set of sets) {
      if (set.red) { bag.red = Math.max(bag.red, set.red) }
      if (set.green) { bag.green = Math.max(bag.green, set.green) }
      if (set.blue) { bag.blue = Math.max(bag.blue, set.blue) }
    }

    sum += bag.red * bag.green * bag.blue
  }
  return sum
}
