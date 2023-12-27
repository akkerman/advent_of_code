const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

function main () {
  /** @type {string[][]} */
  const lines = []

  rl.on('line', line => {
    line = line.split('')

    lines.push(line)
  })

  rl.on('close', () => {
    const start = [0, lines[0].indexOf('.')]
    const end = [lines.length - 1, lines[lines.length - 1].indexOf('.')]
    console.log('partOne', partOne(lines, start, end))
    console.log('partTwo', partTwo(lines, start, end))
  })
}

main()

function makeGetNeighbours (grid) {
  /** @type {(row:number,col:number, allowed:string) => [number,number]} */
  const C = (row, col, allowed) => {
    const char = grid[row] && grid[row][col]
    if (allowed.includes(char)) {
      return [row, col]
    }
  }
  return getNeighbours
  function getNeighbours ([row, col]) {
    return [
      C(row, col + 1, '.>'),
      C(row, col - 1, '.<'),
      C(row + 1, col, '.v'),
      C(row - 1, col, '.^'),
    ].filter(Boolean)
  }
}

function partOne (lines, start, end) {
  const endString = end.toString()
  const getNeighbours = makeGetNeighbours(lines)

  const queue = []
  queue.push({ coord: start, prev: '', steps: 0 })

  let max = 0
  let visiting
  while ((visiting = queue.pop())) {
    if (visiting.coord.toString() === endString) {
      max = Math.max(max, visiting.steps)
      continue
    }
    for (const coord of getNeighbours(visiting.coord)) {
      if (coord.toString() === visiting.prev.toString()) {
        // do not step back
        continue
      }
      queue.push({
        coord,
        prev: visiting.coord,
        steps: visiting.steps + 1,
      })
    }
  }

  return max
}

function partTwo (lines) {
  return 'todo'
}
