const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

/** Possible conditions */
const COND = {
  damaged: '#',
  operational: '.',
  unknown: '?',
}

/**
 * @typedef {string} ConditionRecord
 * @typedef {number[]} DamagedGroups
 * @typedef {[ConditionRecord, DamagedGroups][]} SpringField
 */

function main () {
  /** @type {SpringField} */
  const conditionReport = []

  rl.on('line', line => {
    const [record, nums] = line.split(' ')

    conditionReport.push([
      record,
      nums.split(',').map(int),
    ])
  })

  rl.on('close', () => {
    console.log('partOne', partOne(conditionReport))
    console.log('partTwo', partTwo(conditionReport))
  })
}

main()

/** @type {(cr:ConditionRecord) => DamagedGroups} */
function analyze (cr) {
  return cr.split(COND.operational).map(l => l.length).filter(v => v !== 0)
}

/**
 * Generate a possible fixed ConditionRecord from a damaged one
 * @type {(cr:ConditionRecord) => ConditionRecord }
*/
function * permutate (cr) {
  const id = cr.indexOf(COND.unknown)
  if (id === -1) {
    yield cr
  } else {
    yield * permutate(cr.replace(COND.unknown, COND.operational))
    yield * permutate(cr.replace(COND.unknown, COND.damaged))
  }
}

/** @type {(dr1: DamagedGroups, dg2: DamagedGroups) => boolean} */
function equalDamage (dg1, dg2) {
  if (dg1.length !== dg2.length) return false
  return dg1.toString() === dg2.toString()
}

/** @type {(_:[ConditionRecord, DamagedGroups]) => number } */
function numArrangements ([cr, dg]) {
  let sum = 0
  for (const p of permutate(cr)) {
    if (equalDamage(dg, analyze(p))) {
      sum += 1
    }
  }
  return sum
}

/** @type {(springField: SpringField) => number} */
function partOne (springField) {
  return springField.map(numArrangements).reduce(sum)
}

function partTwo (springField) {
  return 'todo'
}
