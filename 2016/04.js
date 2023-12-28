const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

const { counter } = require('../utils')

function main () {
  const lines = []

  const re = /(.+)-(\d+)\[([a-z]+)\]/

  rl.on('line', line => {
    const [, name, id, checksum] = re.exec(line)
    lines.push({ name, id: Number(id), checksum })
  })

  rl.on('close', () => {
    console.log('partOne', partOne(lines))
    console.log('partTwo', partTwo(lines))
  })
}

main()

function isReal ({ name, checksum }) {
  const arr = name.replaceAll('-', '')
  const counted = counter(arr)

  const sorter = (a, b) => {
    const diff = b[1] - a[1]
    if (diff !== 0) return diff
    return a[0] < b[0] ? -1 : 1
  }

  return R.pipe(
    Object.entries,
    R.sort(sorter),
    R.map(R.head),
    R.join(''),
    R.take(5),
    R.equals(checksum),
  )(counted)
}

function partOne (lines) {
  return R.zipWith(
    (real, id) => real ? id : 0,
    R.map(isReal, lines),
    R.map(R.prop('id'), lines),
  ).reduce(sum)
}

function partTwo (lines) {
  const lowerA = 'a'.charCodeAt(0)
  const rotateLetter = shift => letter => {
    if (letter === '-') return ' '

    let n = letter.charCodeAt(0) - lowerA
    n = (n + shift) % 26 + lowerA

    return String.fromCharCode(n)
  }

  function decrypt (room) {
    const { name, id } = room
    const decrypted = name.split('').map(rotateLetter(id)).join('')
    return {
      ...room,
      name: decrypted,
    }
  }

  return R.pipe(
    R.filter(isReal),
    R.map(decrypt),
    R.find(r => r.name.includes('northpole')),
    R.prop('id'),
  )(lines)
}
