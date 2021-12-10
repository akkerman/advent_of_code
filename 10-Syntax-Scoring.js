
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on("line", data => {
  const line = data.split('')

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

const openclose = {
'(':')',
'[':']',
'{':'}',
'<':'>',
}

const opens = Object.keys(openclose)

const score = {
')': 3,
']': 57,
'}': 1197,
'>': 25137,
}

function validate(line) {
  const stuff = []
  for (let c of line) {
    if (opens.includes(c))  {
      stuff.push(openclose[c])
      continue
    }

    if (stuff.length === 0) {
      console.log(`Expected nothing but found ${c} instead`)
      return score[ c ]
    }

    expected = stuff.pop()

    if (expected !== c) {
      console.log(`Expected ${expected} but found ${c} instead`)
      return score[ c ]
    }
  }

  // if (stuff.length > 0) {
  //   throw `Expected ${stuff} but found nothing`
  // }

  return 0
}

function partOne(lines) {
  return lines.map(validate).reduce((a,b)=>a+b)
}

function partTwo(lines) {
  return 'todo'
}
