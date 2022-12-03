const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const prio = s => (s === s.toUpperCase())
  ? s.charCodeAt(0) - 'A'.charCodeAt(0) + 27
  : s.charCodeAt(0) - 'a'.charCodeAt(0) + 1

function partOne (lines) {
  return R.sum(
    R.map(
      R.pipe(data => [
        data.slice(0, data.length / 2).split(''),
        data.slice(data.length / 2).split(''),
      ],
      ([left, right]) => left.find(c => right.includes(c)),
      prio,
      ),
    )(lines))
}

function partTwo (lines) {
  const groups = lines.reduce((acc, curr, idx) => {
    if ((idx) % 3 === 0) {
      acc.push([curr])
    } else {
      acc[acc.length - 1].push(curr)
    }

    return acc
  }, [])

  const groupItems = groups.map(sacks => {
    const items = sacks[0].split('').filter(i => sacks[1].includes(i))
    return items.filter(i => sacks[2].includes(i))
  }).map(R.head)

  return R.sum(R.map(prio, groupItems))
}
