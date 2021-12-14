const {zipWith,zip} = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let polymerTemplate
const pairInsertionRules = {}

rl.on("line", data => {
  if (data === "") return
  if (!data.includes("->")) {
    polymerTemplate = data.split('')
    return
  }

  const [rule, insert] = data.split(" -> ")

  pairInsertionRules[rule]=insert
})

rl.on('close', () => {
  console.log('partOne', partOne(pairInsertionRules))
  console.log('partTwo', partTwo(pairInsertionRules))
})

function applyRules(polymerTemplate) {
  const newTemplate = []
  newTemplate.push(polymerTemplate[0])
  for (let i=1; i<polymerTemplate.length; i+=1) {
    const next = polymerTemplate[i]
    const match = newTemplate[newTemplate.length-1] + next
    const insert = pairInsertionRules[match] || ""
    newTemplate.push(insert)
    newTemplate.push(next)
  }
  return newTemplate
}

function count(polymerTemplate) {
  return polymerTemplate.reduce((counter, elem) => {
    if (!counter[elem]) {
      counter[elem]=0
    }
    counter[elem]+=1
    return counter
  }, {})
}

function partOne() {
  let template = polymerTemplate
  for (let times = 0; times < 10; times+=1) 
    template = applyRules(template)

  const charCount = count(template)
  const counts = Object.values(charCount)
  return Math.max(...counts) - Math.min(...counts)
}

function constructPairs(polymerTemplate) {
  const pairs = {}
  for (idx=1; idx<polymerTemplate.length; idx+=1) {
    const pair = polymerTemplate[idx-1]+polymerTemplate[idx]
    if (!pairs[pair]) {
      pairs[pair] = 0
    }
    pairs[pair] += 1
  }

  return pairs
}

function pairCountsToCharCount(pairCounts) {
  const fstCharCount = {}
  const sndCharCount = {}

  for (const [pair, count] of Object.entries(pairCounts)) {
    const [fst, snd] = pair.split('')
    if (!fstCharCount[fst]) fstCharCount[fst]=0
    if (!sndCharCount[snd]) sndCharCount[snd]=0
    fstCharCount[fst]+=count
    sndCharCount[snd]+=count
  }
  return {...fstCharCount, ...sndCharCount}
}

function applyRules2(pairCounts) {
  const newPairCounts={}
  for (const [pair, count] of Object.entries(pairCounts)) {
    const insert = pairInsertionRules[pair]
    if (!insert) continue
    const [fst, snd] = pair.split('')

    if (!newPairCounts[fst+insert]) {
      newPairCounts[fst+insert]=0
    }
    newPairCounts[fst+insert]+=count

    if (!newPairCounts[insert+snd]) {
      newPairCounts[insert+snd]=0
    }
    newPairCounts[insert+snd]+=count
  }
  return newPairCounts
}

function partTwo() {
  let pairCounts = constructPairs(polymerTemplate)

  for (let times = 0; times < 40; times+=1) 
    pairCounts = applyRules2(pairCounts)

  const charCount = pairCountsToCharCount(pairCounts)// one -off error, first character
  const counts = Object.values(charCount)
  return Math.max(...counts) - Math.min(...counts)
  return charCount
}
