const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on('line', data => {
  const line = data

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(list(lines)))
  console.log('partTwo', partTwo(list(lines)))
})

function list (lines) {
  let path = []
  let listing = false
  const dirs = {}

  for (const line of lines) {
    if (line.includes('$ cd')) {
      listing = false
      const dir = line.split(' ').pop()
      if (dir === '/') {
        path = []
      } else if (dir === '..') {
        path.pop()
      } else {
        path.push(dir)
      }
      continue
    }

    if (line.includes('$ ls')) {
      listing = true
      continue
    }
    if (listing) {
      const [size, name] = line.split(' ')
      if (size === 'dir') { // oeps vergeten
        const pathstr = [...path, name].join('/')
        if (!dirs[pathstr]) dirs[pathstr] = 0
        continue
      }

      const pathstr = path.join('/')

      if (!dirs[pathstr]) {
        dirs[pathstr] = 0
      }

      dirs[pathstr] += parseInt(size)
    }
  }

  return dirs
}

const sort = dirs => Object.keys(dirs).sort((a, b) => b.split('/').length - a.split('/').length)

const sum = dirs => {
  for (const path of sort(dirs)) {
    if (path === '') continue
    const parent = path.split('/').slice(0, -1).join('/')
    if (!dirs[parent]) dirs[parent] = 0
    dirs[parent] += dirs[path]
  }
}

function partOne (dirs) {
  sum(dirs)

  delete dirs['']
  return Object.values(dirs).filter(size => size <= 100000).reduce((a, b) => a + b)
}

function partTwo (dirs) {
  sum(dirs)

  const unused = 70000000 - dirs['']
  const freeup = 30000000 - unused
  return Math.min(...Object.values(dirs).filter(size => size > freeup))
}
