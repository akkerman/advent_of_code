const U = require('../utils')
const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

const logField = lines => {
  for (const line of lines) {
    log(line)
  }
}

const EMPTY_SPACE = '.'
const GALAXY = '#'

function main () {
  const lines = []

  rl.on('line', line => {
    lines.push(line)
  })

  rl.on('close', () => {
    console.log('partOne', partOne(lines))
    console.log('partOneAlt', partOneAlt(lines))
    console.log('partTwo', partTwo(lines))
  })
}

main()

function expand (lines) {
  const expanded = []

  for (const line of lines) {
    expanded.push(line)

    if (!line.includes(GALAXY)) {
      expanded.push(line)
    }
  }

  let c = 0
  while (true) {
    if (!expanded[0][c]) break
    let expand = true
    for (let r = 0; r < expanded.length; r += 1) {
      if (expanded[r][c] === GALAXY) {
        expand = false
        break
      }
    }
    if (expand) {
      for (let r = 0; r < expanded.length; r += 1) {
        expanded[r] = `${expanded[r].slice(0, c)}${EMPTY_SPACE}${expanded[r].slice(c)}`
      }
      c += 1
    }

    c += 1
  }
  return expanded
}

function extractGalaxyCoords (lines) {
  const coords = []
  for (let r = 0; r < lines.length; r += 1) {
    coords.push(...findOccurences(GALAXY, lines[r])
      .map(c => [r, c]),
    )
  }
  return coords
}

function findOccurences (str, line) {
  const rgx = new RegExp(str, 'gi')
  const indices = []
  let result = {}
  while ((result = rgx.exec(line))) {
    indices.push(result.index)
  }
  return indices
}

const manhattanDistance = ([x1, y1], [x2, y2]) =>
  Math.abs(x1 - x2) + Math.abs(y1 - y2)

function partOne (lines) {
  const coords = extractGalaxyCoords(expand(lines))
  const distances = []
  for (let i = 0; i < coords.length; i += 1) {
    const start = coords[i]
    for (const end of coords.slice(i + 1)) {
      distances.push(manhattanDistance(start, end))
    }
  }

  return distances.reduce(sum)
}

function solve (lines, factor) {
  const shift = factor - 1
  const emptyRows = lines.reduce((rows, line, idx) =>
    (line.includes(GALAXY)) ? rows : [...rows, idx]
  , [])
  const emptyCols = []
  for (let c = 0; c < lines[0].length; c += 1) {
    let isEmpty = true
    for (let r = 0; r < lines.length; r += 1) {
      if (lines[r][c] === GALAXY) {
        isEmpty = false
        break
      }
    }
    if (isEmpty) {
      emptyCols.push(c)
    }
  }

  const coords = extractGalaxyCoords(lines)
    .map(([r, c]) => [
      r + emptyRows.filter(id => id < r).length * shift,
      c + emptyCols.filter(id => id < c).length * shift,
    ])

  const distances = []
  for (let i = 0; i < coords.length; i += 1) {
    const start = coords[i]
    for (const end of coords.slice(i + 1)) {
      distances.push(manhattanDistance(start, end))
    }
  }

  return distances.reduce(sum)
}

function partOneAlt (lines) {
  return solve(lines, 2)
}

function partTwo (lines) {
  return solve(lines, 1_000_000)
}
