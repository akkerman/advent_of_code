const fs = require('node:fs')
const exec = require('child_process').exec
const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

function main () {
  /** @type {Map<string, string[]} */
  const wires = new Map()

  rl.on('line', line => {
    const [from, to] = line.split(': ')

    wires.set(from, to.split(' '))
  })

  rl.on('close', () => {
    console.log('partOne', partOne(wires))
    console.log('partTwo', partTwo(wires))
  })
}

main()

function createGraph (wires) {
  // capture in a 25.dot file
  // neato 25.dot -T svg > 25.svg
  // open in browser

  const file = fs.createWriteStream('./25.dot')

  file.write('graph aoc {')
  for (const [from, dest] of wires.entries()) {
    for (const to of dest) {
      file.write(`${from}--${to};`)
    }
  }
  file.write('}\n')
  file.end()

  exec('neato 25.dot -T svg > 25.svg')
}

/** @type {(wires:Map<string, string[]=>number} */
function partOne (wires) {
  createGraph(wires)

  // make bidirectional
  for (const [from, to] of wires.entries()) {
    for (const t of to) {
      if (!wires.has(t)) {
        wires.set(t, [])
      }
      if (!wires.get(t).includes(from)) {
        wires.get(t).push(from)
      }
    }
  }

  // cut the wires
  const cutWires =
    wires.size < 100
      ? [['hfx', 'pzl'], ['bvb', 'cmg'], ['nvd', 'jqt']] // example
      : [['fvm', 'ccp'], ['lhg', 'llm'], ['thx', 'frl']] // input

  for (const [a, b] of cutWires) {
    wires.set(a, wires.get(a).filter(w => w !== b))
    wires.set(b, wires.get(b).filter(w => w !== a))
  }

  // visit one of the segments
  const visited = new Set()
  const start = [...wires.keys()][0]
  const queue = [start]
  let wire

  while ((wire = queue.pop())) {
    if (visited.has(wire)) continue
    visited.add(wire)
    const neighbours = wires.get(wire)

    for (const nb of neighbours) {
      if (visited.has(nb)) continue
      queue.push(nb)
    }
  }

  return (wires.size - visited.size) * visited.size
}

/** @type {(wires:Record<string, string[]=>number} */
function partTwo (wires) {
  return ''
}
