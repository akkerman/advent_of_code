const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

function includes(str, substr) {
  const diff = R.difference(substr.split(''), str.split('')) 
  return diff.length === 0
}

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

const sortStr = str => str.split('').sort().join('')

function analyze(signalPatterns) {

    one = signalPatterns.find(s => s.length === 2)
    four = signalPatterns.find(s => s.length === 4)
    seven = signalPatterns.find(s => s.length === 3)
    eight = signalPatterns.find(s => s.length === 7)

    six = signalPatterns.find(s => s.length === 6 && (!s.includes(one.charAt(0)) || !s.includes(one.charAt(1))))

    three = signalPatterns.find(s => s.length === 5 && includes(s, one))
    nine = signalPatterns.find(s => s.length === 6 && includes(s, three))

    zero = signalPatterns.find(s => s.length === 6 && s !== six && s !== nine)
    five = signalPatterns.find(s => s.length == 5 && includes(nine, s) && s !== three)
    two = signalPatterns.find(s => s.length == 5 && s !== three && s !== five)

  const analysis = {
    zero, one, two, three, four, five, six, seven, eight, nine
  }

  return Object.values(analysis).reduce((dict, v,i) => {dict[sortStr(v)] = i; return dict}, {})
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
