const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

function parseInstructionIntoFunctions (instr) {
  if (!instr.includes(':')) return () => instr

  const [, name, operator, value, next] = /([xmas])([<>])(\d+):(.+)/.exec(instr)

  const v = int(value)

  if (operator === '<') {
    return (partName, partValue) => name === partName && partValue < v ? next : undefined
  }
  return (partName, partValue) => name === partName && partValue > v ? next : undefined
}

/**
 * @typedef {{
 * name: string
 * operator: string
 * value: string
 * next: string
 * }|{next:string}} Rule
 *
 * @typedef {Rule[]} WorkFlow
 */

/** @type {(instr:string) => Rule} */
function parseInstructionIntoValues (instr) {
  if (!instr.includes(':')) return { next: instr }

  const [, name, operator, v, next] = /([xmas])([<>])(\d+):(.+)/.exec(instr)

  const value = int(v)

  return { name, operator, value, next }
}

function main () {
  const instructionFunctions = {}
  /** @type {Record<string,WorkFlow> } */
  const workflows = {}
  const parts = []

  let instructionParsing = true
  rl.on('line', line => {
    line = line.toString()

    if (line === '') {
      instructionParsing = false
      return
    }

    if (instructionParsing) {
      const [, name, i] = /(.+){(.+)}/.exec(line)

      const instr = i.split(',')
      instructionFunctions[name] = instr.map(parseInstructionIntoFunctions)
      workflows[name] = instr.map(parseInstructionIntoValues)
      return
    }

    const part = JSON.parse(
      line.replaceAll('=', ':').replace(/([xmas])/g, '"$1"'),
    )

    parts.push(part)
  })

  rl.on('close', () => {
    console.log('partOne', partOne(instructionFunctions, parts))
    console.log('partTwo', partTwo(workflows, parts))
  })
}

main()

// x: Extremely cool looking
// m: Musical (it makes a noise when you hit it)
// a: Aerodynamic
// s: Shiny

function partOne (flows, parts) {
  function workflow (part) {
    let flowName = 'in'

    while (!'AR'.includes(flowName)) {
      for (const fn of flows[flowName]) {
        let answ
        for (const [name, value] of Object.entries(part)) {
          answ = fn(name, value)
          if (answ) {
            break
          }
        }
        if (answ) {
          flowName = answ
          break
        }
      }
    }

    return flowName === 'A'
  }

  return parts.filter(workflow).map(part => Object.values(part).reduce(sum)).reduce(sum)
}

/**
 * @typedef {{
 * x: [number,number]
 * m: [number,number]
 * a: [number,number]
 * s: [number,number]
 * flowName: string
 * ruleId: number
 * }} RangedPart
 */

/** @type {(instructions:Record<string,WorkFlow>) => number} */
function partTwo (flows) {
  /** @type {RangedPart[]} */
  const queue = [{
    x: [1, 4000],
    m: [1, 4000],
    a: [1, 4000],
    s: [1, 4000],
    flowName: 'in',
    ruleId: 0,
  }]

  let combinations = 0
  let instr
  while ((instr = queue.shift())) {
    const { flowName, ruleId, ...xmas } = instr
    if (flowName === 'R') continue

    if (flowName === 'A') {
      combinations += Object.values(xmas).reduce((p, [start, end]) => p * (end - start + 1), 1)
      continue
    }

    const rule = flows[flowName][ruleId]
    if (!rule.name) {
      queue.push({ ...xmas, flowName: rule.next, ruleId: 0 })
      continue
    }

    const [start, end] = xmas[rule.name]
    let match, nope

    if (rule.operator === '<') {
      if (end < rule.value) {
        queue.push({ ...xmas, flowName: rule.next, ruleId: 0 })
      } else {
        const range = [start, rule.value - 1, rule.value, end]
        match = range.slice(0, 2)
        nope = range.slice(2)
      }
    } else { // rule.operator === '>'
      if (start > rule.value) {
        queue.push({ ...xmas, flowName: rule.next, ruleId: 0 })
      } else {
        const range = [start, rule.value, rule.value + 1, end]
        nope = range.slice(0, 2)
        match = range.slice(2)
      }
    }

    queue.push({ ...xmas, [rule.name]: match, flowName: rule.next, ruleId: 0 })
    queue.push({ ...xmas, [rule.name]: nope, flowName: flowName, ruleId: ruleId + 1 })
  }

  return combinations
}
