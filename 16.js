const log = console.log
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

rl.on("line", data => {
  const line = data

  lines.push(line)
})

const hextobin = {
  "0":"0000",
  "1":"0001",
  "2":"0010",
  "3":"0011",
  "4":"0100",
  "5":"0101",
  "6":"0110",
  "7":"0111",
  "8":"1000",
  "9":"1001",
  "A":"1010",
  "B":"1011",
  "C":"1100",
  "D":"1101",
  "E":"1110",
  "F":"1111",
}

function literal(message) {
  log('literal', message)
  let data = [] 
  let idx = 0
  let rest 
  while (true) {
    const part = message.slice(idx, idx+5)
    data.push(part.slice(1))
    if (part.charAt(0) === '0') {
      rest = message.slice(idx+5)
      break
    }
    idx+=5
  }
  const num = Number.parseInt(data.join(''), 2)
  return [num].concat(analyze(rest))
} 

function operator(message) {
  log('operator', message)
  const lengtTypeId = message.charAt(0)
  const bitsInLength = lengtTypeId === '0' ? 15 : 11
    const lengthBits = message.slice(1, bitsInLength+1)
    const length = Number.parseInt(lengthBits,2)
  if (bitsInLength === 15) {
    console.log('total package length', length)
    let start = bitsInLength+1
    let stop = start + length
    log('operator', {bitsInLength, lengthBits, length})
    return analyze(message.slice(start,stop))
  } else {
    console.log('number of packets', length)
    let start = bitsInLength+1
    let m = message.slice(start)
    let data = []
    for (let packetId=0; packetId<length; packetId+=1) {
      data=data.concat(analyze(m.slice(0,11)))
      m=m.slice(11)
    }
    return data
  }
}


const versions = []
function analyze(message) {
  log('analyze', message)
  const version = message.slice(0,3)
  versions.push(version)
  
  const id = message.slice(3,6)

  if (id === '100') { // 4
    return literal(message.slice(6))
  } 

  console.log('delegate to operator', id, Number.parseInt(id,2))
  return operator(message.slice(6))
}

rl.on('close', () => {
  console.log('partOne', partOne(lines[0]))
  console.log('partTwo', partTwo(lines[0]))
})

function partOne(hex) {
  const message = hex.split('').map(c => hextobin[c]).join('')
  const result =  analyze(message)
  console.log(JSON.stringify(result, null, 2))
  const versionSum = versions.filter(_=>_).map(i=>Number.parseInt(i,2)).reduce((a,b)=>a+b) 
  console.log(versions)
  return versionSum
}

function partTwo(lines) {
  return 'todo'
}

module.exports = {
  literal,
  operator,
  partOne,
  partTwo,
}
