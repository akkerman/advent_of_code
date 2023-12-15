const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

function main () {
  /** @type string[] */
  const sequence = []

  rl.on('line', line => {
    line = line.toString()

    sequence.push(...line.split(','))
  })

  rl.on('close', () => {
    console.log('partOne', partOne(sequence))
    console.log('partTwo', partTwo(sequence))
  })
}

main()

/** @type {(curr:number, char:string) => number} */
function hash (curr, char) {
  curr += char.charCodeAt(0)
  curr *= 17
  curr %= 256
  return curr
}

/** @type {(step:string) => number} */
function hashStep (step) {
  return step.split('').reduce(hash, 0)
}

/** @type {(sequence:string[]) => number} */
function partOne (sequence) {
  return sequence
    .map(hashStep)
    .reduce(sum)
}

/** @type {(sequence:string[]) => number} */
function partTwo (sequence) {
  return 'todo'
}
