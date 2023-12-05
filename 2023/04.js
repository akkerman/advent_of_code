const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = fn => args => { fn(args); return args } // eslint-disable-line 
const log = console.log // eslint-disable-line
const sum = (a, b) => a + b

const cards = []

rl.on('line', data => {
  const [w, h] = data.replace(/Card.+: /, '').split(' | ')

  const winning = w.trim().split(/ +/).map(s => parseInt(s.trim()))
  const have = h.trim().split(/ +/).map(s => parseInt(s.trim()))

  cards.push([winning, have])
})

rl.on('close', () => {
  console.log('partOne', partOne(cards))
  console.log('partTwo', partTwo(cards))
})

function partOne (cards) {
  return cards
    .map(calculateWorth)
    .reduce(sum)
}

function calculateWorth ([winning, youHave]) {
  return winning
    .reduce((worth, win) => {
      if (!youHave.includes(win)) return worth
      return worth === 0 ? 1 : 2 * worth
    }, 0)
}

function calculateNumberWins ([winning, youHave]) {
  return winning
    .reduce((worth, win) => {
      if (!youHave.includes(win)) return worth
      return worth + 1
    }, 0)
}

function partTwo (cards) {
  const numWins = cards.map(calculateNumberWins)
  const numCards = Array.from(cards, () => 1)

  const maxCards = numCards.length

  for (let idx = 0; idx < maxCards; idx += 1) {
    const wins = numWins[idx]

    for (let j = idx + 1; j < idx + 1 + wins && j < maxCards; j += 1) {
      numCards[j] += numCards[idx]
    }
  }

  return numCards.reduce(sum)
}
