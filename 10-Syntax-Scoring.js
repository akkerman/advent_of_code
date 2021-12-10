
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on("line", data => {
  const line = data.split('')

  lines.push(line)
})

rl.on('close', () => {
  if (process.env.CORRECT) {
    correctTheProgram(lines)
    return
  } 

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

const incompleteScore ={
  ')': 1,
  ']': 2,
  '}': 3,
  '>': 4,
}

function validatePartOne(line) {
  const stuff = []
  for (let c of line) {
    if (opens.includes(c))  {
      stuff.push(openclose[c])
      continue
    }

    if (stuff.length === 0) {
      // console.log(`Expected nothing but found ${c} instead`)
      return score[ c ]
    }

    expected = stuff.pop()

    if (expected !== c) {
      // console.log(`Expected ${expected} but found ${c} instead`)
      return score[ c ]
    }
  }

  return 0
}

function partOne(lines) {
  return lines.map(validatePartOne).reduce((a,b)=>a+b)
}

function validatePartTwo(line) {
  const stuff = []
  for (let c of line) {
    if (opens.includes(c))  {
      stuff.push(openclose[c])
      continue
    }

    if (stuff.length === 0) {
      // console.log(`Expected nothing but found ${c} instead`)
      return null
    }

    expected = stuff.pop()

    if (expected !== c) {
      // console.log(`Expected ${expected} but found ${c} instead`)
      return null
    }
  }

  const completion = stuff.reverse()
  // console.log(`expected ${completion.join('')}`)

  const score = completion.map(c => incompleteScore[c]).reduce((a,b) => a*5 + b, 0)
  return score
}

function partTwo(lines) {

  const result = lines.map(validatePartTwo).filter(_=>_).sort((a,b)=>a-b)
  const idx = (result.length-1)/2

  return result[idx]
}



function correct(line) {
  const stuff = []
  for (let c of line) {
    if (opens.includes(c))  {
      stuff.push(openclose[c])
      continue
    }

    if (stuff.length === 0) return null

    expected = stuff.pop()

    if (expected !== c) return null
  }

  return line.concat(stuff.reverse())
}
function correctTheProgram(program) {
  program.map(correct).filter(_=>_).map(l=>console.log(l.join('')))
}
