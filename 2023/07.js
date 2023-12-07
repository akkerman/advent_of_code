const { group } = require('console')
const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = fn => args => { fn(args); return args } // eslint-disable-line 
const log = console.log // eslint-disable-line
const sum = (a, b) => a + b
const int = s => parseInt(s.trim())

const lines = []

const cards = 'AKQJT98765432'.split('').reverse()
const cardValue = c => cards.indexOf(c)
const groupHand = hand => Object.values(R.groupBy(R.identity, hand))
const sortLength = gh => gh.map(a => a.length).sort().reverse()

const FIVE_KIND = 50
const FOUR_KIND = 40
const FULL_HOUSE = 32
const THREE_KIND = 30
const TWO_PAIR = 20
const ONE_PAIR = 10
const HIGH_CARD = 1

const groupedHandValue = h => {
  if (h.length === 5) return HIGH_CARD
  if (h.includes(5)) return FIVE_KIND
  if (h.includes(4)) return FOUR_KIND
  if (h.includes(3) && h.includes(2)) return FULL_HOUSE
  if (h.includes(3)) return THREE_KIND
  if (h.includes(2) && h.length === 3) return TWO_PAIR
  if (h.includes(2)) return ONE_PAIR
  throw new Error()
}

const handTypeValue = R.pipe(groupHand, sortLength, groupedHandValue)

const handRanker = (a, b) => {
  const diff = a.typeValue - b.typeValue
  if (diff !== 0) return diff

  const cardValues = R.zip(a.cards.map(cardValue), b.cards.map(cardValue))
  for (const [c1, c2] of cardValues) {
    if (c1 === c2) continue
    return c1 - c2
  }
}

// start JOKER

const cardsJoker = 'AKQT98765432J'.split('').reverse()
const cardValueJoker = c => cardsJoker.indexOf(c)

const groupedHandValueJoker = (h, hand) => {
  const value = groupedHandValue(h)
  const jokers = hand.join('').replace(/[AKQT98765432]/g, '').length
  if (jokers === 0) return value

  switch (value) {
    case FIVE_KIND :
      return FIVE_KIND
    case FOUR_KIND :
      return FIVE_KIND
    case FULL_HOUSE :
      return FIVE_KIND
    case THREE_KIND :
      return FOUR_KIND
    case TWO_PAIR :
      if (jokers === 1) return FULL_HOUSE
      if (jokers === 2) return FOUR_KIND
      throw new Error()
    case ONE_PAIR :
      return THREE_KIND
    case HIGH_CARD :
      return ONE_PAIR
  }
}

const handTypeValueJoker = hand => groupedHandValueJoker(
  sortLength(groupHand(hand)),
  hand,
)

const handRankerJoker = (a, b) => {
  const diff = a.typeValueJoker - b.typeValueJoker
  if (diff !== 0) return diff

  const cardValues = R.zip(a.cards.map(cardValueJoker), b.cards.map(cardValueJoker))
  for (const [c1, c2] of cardValues) {
    if (c1 === c2) continue
    return c1 - c2
  }
}

// end JOKER

rl.on('line', line => {
  const [hand, bid] = line.split(' ')

  const data = {
    hand,
    bid: int(bid),
    cards: hand.split(''),
    groups: groupHand(hand),
    typeValue: handTypeValue(hand.split('')),
    typeValueJoker: handTypeValueJoker(hand.split('')),
  }

  const jokers = hand.replace(/[AKQT98765432]/g, '').length
  if (jokers > 0) {
    data.jokers = jokers
  }

  lines.push(data)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne (lines) {
  return lines.sort(handRanker)
    .map(({ bid }, rank) => bid * (rank + 1))
    .reduce(sum)
}

function partTwo (lines) {
  return lines
    .sort(handRankerJoker)
    .map(({ bid }, rank) => bid * (rank + 1))
    .reduce(sum)
}
