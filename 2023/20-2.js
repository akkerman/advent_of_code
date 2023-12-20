const { lcm } = require('../utils.js')
const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

/**
 * @typedef {'high'|'low'} Pulse
 * @typedef {string} Name
 * @typedef {'%'|'&'|undefined} ModuleType
 * @typedef {{
 *   type: ModuleType
 *   name: Name
 *   dest: Name[]
 *   inputs: Record<Name,Pulse>
 * }} Base
 *
 * @typedef {Base&{
 *   type: '%'
 *   state: 'on'|'off'
 *   flipped: boolean
 * }} FlipFlop
 *
 * @typedef {Base&{
 *   type: '&',
 * }} Conjunction
 *
 * @typedef {Base&{
 *   type: undefined
 * }} Broadcaster
 *
 * @typedef {FlipFlop|Conjunction|Broadcaster} Module
 *
 * @typedef {Record<Name,Module>} Configuration
 */

function main () {
  /** @type {Configuration} */
  const configuration = {}

  const re = /([%&])?(.+) -> (.+)/

  rl.on('line', line => {
    const [, type, name, dest] = re.exec(line)

    const module = { type, name, dest: dest.split(', '), inputs: {} }

    if (module.type === '%') {
      module.state = 'off'
      module.flipped = false
    }

    configuration[name] = module
  })

  rl.on('close', () => {
    solve(R.clone(configuration))
  })
}

main()

/** @type {(configuration:Configuration) => Number}) */
function solve (configuration) {
  addTerminators()
  initializeConjunctions()

  const counter = {
    high: 0,
    low: 0,
  }

  let module

  const prev = {
    vm: [],
    lm: [],
    jd: [],
    fv: [],
  }

  let done = false
  let i = 0
  while (!done) {
    i += 1
    const queue = [configuration.broadcaster]
    while ((module = queue.shift())) {
      if (module.name === 'output') {
        continue
      }

      const pulse = pulseToSend(module)
      if (!pulse) continue
      counter[pulse] += module.dest.length

      for (const name of module.dest) {
        const destModule = configuration[name]
        if (name === 'zg' &&
          Object.values(destModule.inputs).some(v => v === 'high')) {
          Object.entries(destModule.inputs)
            .filter(([n, p]) => p === 'high')
            .forEach(([n]) => {
              prev[n] = R.uniq(prev[n].concat(i))
            })

          if (Object.values(prev).every(v => v.length >= 2)) {
            done = true
            break
          }
        }
        receivePulse(destModule, pulse, module.name)

        // log(module.name, `-${pulse}->`, name)

        queue.push(destModule)
      }
    }
  }

  log(prev)

  log(Object.values(prev).map(v => v[0]).reduce(lcm))

  return counter.high * counter.low

  /** @type {(module:Module) => Pulse|undefined} */
  function pulseToSend (module) {
    switch (module.type) {
      case '%':
        if (!module.flipped) return
        module.flipped = false
        return module.state === 'on' ? 'high' : 'low'
      case '&':
        return Object.values(module.inputs).every(p => p === 'high')
          ? 'low'
          : 'high'
      default:
        return 'low'
    }
  }

  /** @type {(module:Module, pulse:Pulse, from:string) => void} */
  function receivePulse (module, pulse, from) {
    switch (module.type) {
      case '%':
        module.inputs[from] = pulse
        if (pulse === 'low') {
          module.state = module.state === 'on' ? 'off' : 'on'
          module.flipped = true
        }

        break
      case '&':
        module.inputs[from] = pulse
        break
      default:
        return 'low'
    }
  }

  function initializeConjunctions () {
    const modules = Object.values(configuration)
    /** @type {Conjunction[]} */
    const conjunctions = modules
      .filter(m => m.type === '&')

    for (const con of conjunctions) {
      const senders = modules
        .filter(m => m.dest.includes(con.name))
      con.inputs = senders.reduce(
        (acc, m) => ({ ...acc, [m.name]: 'low' }),
        {},
      )
    }
  }

  function addTerminators () {
    const moduleNames = Object.values(configuration)
      .flatMap(R.prop('dest'))

    const terminatorNames = R.difference(moduleNames, Object.keys(configuration))

    for (const name of terminatorNames) {
      configuration[name] = { name, dest: [] }
    }
  }
}

function createGraph (config) {
  const modules = Object.values(config)
  log('```mermaid')
  log('graph TD')
  log('broadcaster')
  for (const module of modules) {
    if (module.name === 'broadcaster') continue
    const { name, type } = module
    log(`${name}((${type || ''}${name}))`)
  }
  for (const module of modules) {
    const { name, dest } = module
    for (const to of dest) {
      log(`${name}-->${to}`)
    }
  }
  log('```')

  // &fv -> &zg
  // &jd -> &zg
  // &lm -> &zg
  // &vm -> &zg
  // &zg -> rx
}

/** @type {(configuration:Configuration) => Number}) */
function partTwo (configuration) {
  // createGraph(configuration)
}
