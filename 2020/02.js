const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let numValidPartOne = 0
let numValidPartTwo = 0

rl.on("line", data => {
  const parsed = parse(data)
  if (isValidPartOne(parsed)) {
    numValidPartOne += 1
  }
  if (isValidPartTwo(parsed)) {
    numValidPartTwo += 1
  }
})

rl.on('close', () => {
  console.log('partOne', numValidPartOne)
  console.log('partTwo', numValidPartTwo)
})


function isValidPartOne(data) {
  const chars = data.password.split('').filter(x => x===data.char)
  return data.min <= chars.length && chars.length <= data.max
}

function isValidPartTwo(data) {
  const chars = data.password.split('')

  const onMin = chars[data.min-1] === data.char
  const onMax = chars[data.max-1] === data.char

  return (onMin && !onMax) || (!onMin && onMax)

}

function parse(line) {
  const [min,max,char,password] =  line.replace(/[-:]/g,' ').split(/ +/)
  return {
    min:Number.parseInt(min),
    max:Number.parseInt(max),
    char,
    password
  }
}
