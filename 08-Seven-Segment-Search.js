const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const lines = []

const nums = [
  'abcefg', //0
  'cf',     //1
  'acdeg',  //2
  'acdfg',  //3
  'bcdf',   //4
  'abdfg',  //5
  'abdefg', //6
  'acf',    //7
  'abcdefg',//8
  'abcdfg', //9
]

rl.on("line", data => {
  const line = data.split(' | ')[1].split(' ')

  lines.push(line)
})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function partOne(lines) {
  let sum = 0
  for (const line of lines) {
    sum+=line.filter(line => [2,3,4,7].includes(line.length)).length
  }

  return sum
}

function partTwo(lines) {
  return 'todo'
}
