//
// const readline = require('readline')
// const rl = readline.createInterface({ input: process.stdin })
//
// const lines = []
//
// rl.on('line', data => {
//  const line = data
//
//  lines.push(line)
// })
//
// rl.on('close', () => {
//  console.log('partOne', partOne(lines[0]))
//  console.log('partTwo', partTwo(lines[0]))
// })

const int = i => Number.parseInt(i, 2)

const hextobin = {
  0: '0000',
  1: '0001',
  2: '0010',
  3: '0011',
  4: '0100',
  5: '0101',
  6: '0110',
  7: '0111',
  8: '1000',
  9: '1001',
  A: '1010',
  B: '1011',
  C: '1100',
  D: '1101',
  E: '1110',
  F: '1111'
}

function literal (message) {
  const version = int(message.slice(0, 3))
  const typeId = int(message.slice(3, 6))
  if (!typeId === '100') throw new Error('wrong type id' + typeId)

  const data = []
  let idx = 6
  let rest
  while (true) {
    const part = message.slice(idx, idx + 5)
    data.push(part.slice(1))
    if (part.charAt(0) === '0') {
      rest = message.slice(idx + 5)
      break
    }
    idx += 5
  }
  const value = Number.parseInt(data.join(''), 2)
  return [{ version, typeId, value }, rest]
}

function operator (message) {
  const version = int(message.slice(0, 3))
  const typeId = int(message.slice(3, 6))
  const lengthTypeId = message.slice(6, 7)
  const bitsInLength = lengthTypeId === '0' ? 15 : 11
  const lengthBits = message.slice(7, bitsInLength + 7)
  const length = int(lengthBits, 2)
  if (bitsInLength === 15) { // 0
    const start = bitsInLength + 7
    const stop = start + length
    const packets = []
    let m = message.slice(start, stop)
    const myRest = message.slice(stop)
    while (m) {
      const [packet, rest] = analyze(m)
      packets.push(packet)
      m = rest
    }
    return [{ version, typeId, lengthTypeId, packets }, myRest]
  } else { // 1
    const start = bitsInLength + 7
    let m = message.slice(start)
    let packets = []
    for (let packetId = 0; packetId < length; packetId += 1) {
      const [p, rest] = analyze(m)
      packets = packets.concat(p)
      m = rest
    }
    return [{ version, typeId, lengthTypeId, packets }, m]
  }
}

function analyze (message) {
  if (!message || message.length < 6) return []
  const id = message.slice(3, 6)

  if (id === '100') { // 4
    return literal(message)
  }

  return operator(message)
}

function versionSum (packet) {
  if (!packet) return 0
  if (packet.packets) {
    return packet.packets.map(versionSum).reduce((a, b) => a + b, packet.version)
  }

  return packet.version
}
function partOne (hex) {
  const message = hex2message(hex)
  const result = analyze(message)
  return versionSum(result[0])
}

function partTwo (lines) {
  return 'todo'
}

function hex2message (hex) {
  return hex.split('').map(c => hextobin[c]).join('')
}

module.exports = {
  hex2message,
  literal,
  operator,
  analyze,
  partOne,
  partTwo
}

partOne('8A004A801A8002F478')
