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

/** @type {(springField: SpringField) => number} */
function partOneOrig (springField) {
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

  return springField.map(numArrangements).reduce(sum)
}

function solve (springField) {
  const memo = new Map()

  function numArrangements (cr, dg, inGroup) {
    const str = JSON.stringify({ cr, dg, inGroup })
    if (!memo.has(str)) {
      memo.set(str, numArrangements_(cr, dg, inGroup))
    }
    return memo.get(str)
  }

  /** @type {(cr:ConditionRecord, dg: DamagedGroups, inGroup:boolean) => number } */
  function numArrangements_ (cr, dg, inGroup = false) {
    if (cr.length === 0) {
      // alle springs verwerkt, dan moeten alle damaged springs gematched zijn
      return (dg.reduce(sum, 0) === 0) ? 1 : 0
    }

    if (dg.reduce(sum, 0) === 0) {
      // alle damaged groups gemachted dan moet er geen meer over zijn
      return cr.includes(COND.damaged) ? 0 : 1
    }

    const currentSpring = cr[0]
    const group = dg[0]

    if (typeof group !== 'number') {
      throw new Error()
    }

    if (currentSpring === COND.damaged) {
      if (inGroup && group === 0) {
        // als we een damaged group matchen moet er wel nog 1tje over zijn
        return 0
      }

      const newDG = [group - 1, ...dg.slice(1)]

      return numArrangements(cr.slice(1), newDG, true)
    }

    if (currentSpring === COND.operational) {
      if (inGroup && group !== 0) {
        // als we nog in een group zijn maar deze was nog niet helemaal gematched
        return 0
      }

      // als we in een group waren en die is net helemaal gematched, gooi group=0 weg
      const newDG = group === 0 ? dg.slice(1) : dg
      return numArrangements(cr.slice(1), newDG, false)
    }

    // assert currentSpring === COND.unknown

    if (inGroup) {
      if (group === 0) {
        // net klaar met matchen van een group
        // beschouw unknown as operational en separator tussen groups
        return numArrangements(cr.slice(1), dg.slice(1), false)
      }
      // nog bezig met matchen van een damaged group
      const newDG = [group - 1, ...dg.slice(1)]
      return numArrangements(cr.slice(1), newDG, true)
    }

    let total = 0

    // maak een nieuwe group van damaged springs
    const newDG = [group - 1, ...dg.slice(1)]

    total += numArrangements(cr.slice(1), newDG, true)

    // maak de scheiding tussen groepen groter met een operational spring
    total += numArrangements(cr.slice(1), dg, false)

    return total
  }

  return springField.map(([cr, dg]) => numArrangements(cr, dg)).reduce(sum)
}

function partOne (springField) {
  return solve(springField)
}
function partTwo (springField) {
  const unfold = ([cr, dg]) => {
    const cr5 = []
    const dg5 = []
    for (let i = 0; i < 5; i += 1) {
      cr5.push(cr)
      dg5.push(...dg)
    }
    return [cr5.join('?'), dg5]
  }

  return solve(springField.map(unfold))
}
