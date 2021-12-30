const {zipWith} = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const coords = []
const foldInstructions = []
let parseInstructions = false
let maxX = 0
let maxY = 0

rl.on("line", data => {
  if (data === '') {
    parseInstructions=true
    return
  }
  if (parseInstructions) {
    const instruction = data.replace('fold along ','').split('=')
    instruction[1] = Number.parseInt(instruction[1])

    foldInstructions.push(instruction)
    return
  }
  const line = data.split(',').map(i=>Number.parseInt(i))


  maxX = Math.max(line[0], maxX)
  maxY = Math.max(line[1], maxY)
  coords.push(line)
})


rl.on('close', () => {
  createTransparentPaper(coords)
  console.log('partOne', partOne())
  console.log('partTwo', partTwo())
})

const pp = lines => {
  for (const line of lines) {
    console.log(line.join(''))
  }
}

const fold = (arr1, arr2) => zipWith((a,b) => (a === '#' || b === '#') ? '#':'.', arr1, arr2)

function foldUp(arr, num) {
  const upper = arr.slice(0,num)
  const lower = arr.slice(num+1).reverse()
  return zipWith(fold, upper, lower)
}

function foldLeft(arr, num) {
  return arr.map(line=>{ 
    const left = line.slice(0,num)
    const right = line.slice(num+1).reverse()
    return fold(left, right)
  })
}

function createTransparentPaper(coords) {
  const length = (maxY % 2 === 0) ? maxY + 1: maxY +2
  const transparentPaper = Array.from({length}, () => Array(maxX+1).fill('.'))
  for (let [x,y] of coords) {
    transparentPaper[y][x] = '#'
  }
  return transparentPaper
}

function partOne() {
  const paper = createTransparentPaper(coords)
  let folded = paper
  for (let [dir, num] of foldInstructions) {
    folded = dir === 'y' ? foldUp(folded, num) : foldLeft(folded,num)
    break
  }
  const nums = folded.flatMap(arr => arr.map(a => a === '#'?1:0)).reduce((a,b)=>a+b)
  return nums
}

function partTwo() {
  const paper = createTransparentPaper(coords)
  let folded = paper
  for (let [dir, num] of foldInstructions) {
    folded = dir === 'y' ? foldUp(folded, num) : foldLeft(folded,num)
  }
  console.log(pp(folded))
}
