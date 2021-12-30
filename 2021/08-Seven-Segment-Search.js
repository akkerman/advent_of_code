const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on("line", data => {
  const [signalPatterns, outputValues] = data.split(' | ').map(v=>v.split(' '))

  lines.push({signalPatterns, outputValues})
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne(lines) {
  let sum = 0
  for (const {outputValues} of lines) {
    sum+=outputValues.filter(v => [2,3,4,7].includes(v.length)).length
  }

  return sum
}

const includes = (str, substr) => R.difference(substr, str).length === 0
const sortStr = str => str.split('').sort().join('')
const toSortedArrays = str => str.split('').sort()

function analyze(signalPatterns) {
  const patterns = signalPatterns.map(toSortedArrays)

  const one   = patterns.find(s => s.length === 2)
  const four  = patterns.find(s => s.length === 4)
  const seven = patterns.find(s => s.length === 3)
  const eight = patterns.find(s => s.length === 7)
  const three = patterns.find(s => s.length === 5 && includes(s, one))
  const six   = patterns.find(s => s.length === 6 && (!s.includes(one[0]) || !s.includes(one[1])))
  const nine  = patterns.find(s => s.length === 6 && includes(s, three))
  const zero  = patterns.find(s => s.length === 6 && s !== six && s !== nine)
  const five  = patterns.find(s => s.length === 5 && s !== three && includes(nine, s))
  const two   = patterns.find(s => s.length === 5 && s !== three && s !== five)

  const analysis = {
    zero, one, two, three, four, five, six, seven, eight, nine
  }

  return Object.values(analysis).reduce((dict, v,i) => {dict[v.join('')] = i; return dict}, {})
}


function partTwo(lines) {
  let  sum = 0
  for (const {signalPatterns, outputValues} of lines) {
    const dict = analyze(signalPatterns)
    const num = outputValues.map(s => dict[sortStr(s)])
    sum += Number.parseInt(num.join(''))
  }
  return sum
}
