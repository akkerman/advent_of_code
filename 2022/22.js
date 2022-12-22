const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data

  lines.push(line)
})

rl.on('close', () => {
  const path = lines.pop()
  const map = adjustMap(lines)

  console.log('partOne', partOne(map, path))
  console.log('partTwo', partTwo(map, path))
})

/**
 * Put an empty line above, below, to the right and left of the map.
 * This serves two purposes
 * 1. When encountering a space -> wrap around
 * 2. Adjust for the fact dat js has 0 based array and the problemstatement is 1 based
 */
function adjustMap (map) {
  map.pop() // empty line

  const length = map[0].length

  map.unshift(Array(length).fill(' ').join(''))
  map.push(Array(length).fill(' ').join(''))

  return map.map(line => {
    const newLine = ' ' + line + Array(length).fill(' ').join('')
    return newLine.slice(0, length + 2)
  })
}

function * parsePath (path) {
  let steps = ''
  for (const c of path) {
    if (['L', 'R'].includes(c)) {
      yield parseInt(steps)
      yield c
      steps = ''
    } else {
      steps += c
    }
  }
}

const turn = {
  '>': { L: '^', R: 'v' },
  '<': { L: 'v', R: '^' },
  '^': { L: '<', R: '>' },
  v: { L: '>', R: '<' },
}

const score = {
  '>': 0,
  '<': 2,
  '^': 3,
  v: 1,
}

function firstInLine (line) {
  const dot = line.indexOf('.')
  const hash = line.indexOf('#')
  if (![dot, hash].includes(-1)) {
    return dot < hash ? dot : hash
  }
  return (dot !== -1) ? dot : hash
}

function lastInLine (line) {
  const dot = line.lastIndexOf('.')
  const hash = line.lastIndexOf('#')
  if (![dot, hash].includes(-1)) {
    return dot < hash ? hash : dot
  }
  return (dot !== -1) ? dot : hash
}

function firstInColumn (map, c) {
  for (let r = 1; r < map.length; r += 1) {
    if (map[r][c] !== ' ') return r
  }
  throw new Error('only empty tiles?')
}
function lastInColumn (map, c) {
  for (let r = map.length - 1; r > 0; r -= 1) {
    if (map[r][c] !== ' ') return r
  }
  throw new Error('only empty tiles?')
}

function partOne (map, path) {
  let facing = '>'
  let row = 1
  let col = map[row].indexOf('.')

  for (let steps of parsePath(path)) {
    if (['L', 'R'].includes(steps)) {
      facing = turn[facing][steps]
      continue
    }
    let r = row
    let c = col
    while (steps-- > 0) {
      if (facing === '>') {
        c += 1
        if (map[r][c] === '#') break
        if (map[r][c] === ' ') { // wrap to start
          c = firstInLine(map[r])
          if (map[r][c] === '#') break
        }
        col = c
      } else

      if (facing === '<') {
        c -= 1
        if (map[r][c] === '#') break
        if (map[r][c] === ' ') { // wrap to end
          c = lastInLine(map[r])
          if (map[r][c] === '#') break
        }
        col = c
      } else

      if (facing === '^') {
        r -= 1
        if (map[r][c] === '#') break
        if (map[r][c] === ' ') { // wrap to bottom
          r = lastInColumn(map, c)
          if (map[r][c] === '#') break
        }
        row = r
      } else

      if (facing === 'v') {
        r += 1
        if (map[r][c] === '#') break
        if (map[r][c] === ' ') { // wrap to top
          r = firstInColumn(map, c)
          if (map[r][c] === '#') break
        }
        row = r
      }
    }
    console.log(facing, row, col)
  }

  console.log(facing, row, col)

  // too low: 89204
  return 1000 * row + 4 * col + score[facing]
}

function partTwo (lines) {
  return 'todo'
}
