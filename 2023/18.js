const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const tap = R.tap
const log = console.log // eslint-disable-line
const sum = (a,b) => a+b // eslint-disable-line
const mul = (a,b) => a*b // eslint-disable-line
const int = R.pipe(R.trim, parseInt) // eslint-disable-line

const reg = /^(.) (\d+) \(#(.+)\)$/

function main () {
  const digplan = []
  rl.on('line', line => {
    const [_, direction, meters, color] = reg.exec(line)

    digplan.push({ direction, meters: int(meters), color })
  })

  rl.on('close', () => {
    console.log('partOne', partOne(digplan))
    console.log('partTwo', partTwo(digplan))
  })
}

main()

/**
 * @typedef {import('../utils').Coord} Coord
 * @typedef {U|D|L|R} Direction
 */

function partOne (digplan) {
  /** @type {(coord: Coord, direction: Direction) => Coord */
  function move ([r, c], direction, meters) {
    const line = []
    for (let m = 1; m <= meters; m += 1) {
      switch (direction) {
        case 'U':
          line.push([r - m, c])
          break
        case 'D':
          line.push([r + m, c])
          break
        case 'L':
          line.push([r, c - m])
          break
        case 'R':
          line.push([r, c + m])
          break
      }
    }

    return line
  }

  const floor = new Set()

  let maxRow = 0
  let maxCol = 0
  let minRow = Number.MAX_SAFE_INTEGER
  let minCol = Number.MAX_SAFE_INTEGER

  function drawLine () {
    let coord = [0, 0]
    floor.add(coord.toString())
    for (const { direction, meters } of digplan) {
      const line = move(coord, direction, meters)
      coord = line.at(-1)

      maxRow = Math.max(coord[0], maxRow)
      maxCol = Math.max(coord[1], maxCol)
      minRow = Math.min(coord[0], minRow)
      minCol = Math.min(coord[1], minCol)

      line.forEach(c => floor.add(c.toString()))
    }
  }

  function getNeigbours (coord) {
    return 'UDLR'.split('').flatMap(d => move(coord, d, 1))
  }

  function fill () {
    const topPoint = [minRow, minCol]

    while (true) {
      if (floor.has(topPoint.toString())) break
      topPoint[1] += 1
    }
    const [r, c] = topPoint
    const queue = [[r + 1, c + 1]]

    while (true) {
      const next = queue.shift()
      if (!next) break
      const nextStr = next.toString()
      if (floor.has(nextStr)) continue
      const [r, c] = next

      if (r < minRow || r > maxRow || c < minCol || c > maxCol) {
        throw new Error('out of bounds')
      }
      floor.add(nextStr)

      const nbrs = getNeigbours(next)
      for (const nb of nbrs) {
        const str = nb.toString()
        if (floor.has(str)) continue
        queue.push(nb)
      }
    }
  }

  drawLine()
  fill()

  const uitersten = { minRow, minCol, maxRow, maxCol }
  print(floor, uitersten)
  return floor.size
}

function print (floor, { minRow, minCol, maxRow, maxCol }) {
  const height = Math.abs(minRow) + Math.abs(maxRow)
  const width = Math.abs(minCol) + Math.abs(maxCol)

  const rowOffset = Math.abs(minRow)
  const colOffset = Math.abs(minCol)

  const grid = Array.from({ length: height + 1 }, () => Array.from({ length: width + 1 }, () => ' '))
  for (const coord of floor.values()) {
    const [r, c] = coord.split(',').map(int)
    grid[r + rowOffset][c + colOffset] = '#'
  }

  for (const line of grid) {
    log(line.join(''))
  }
}

function partTwo (digPlan) {
  const transl = {
    0: 'R',
    1: 'D',
    2: 'L',
    3: 'U',
  }
  const newDigPlan = digPlan
    .map(({ color }) => ({
      direction: transl[color.at(-1)],
      meters: parseInt(color.slice(0, 5), 16),
    }))

  return newDigPlan
}
