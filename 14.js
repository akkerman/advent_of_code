const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let polymerTemplate
const pairInsertionRules = {}

rl.on("line", data => {
  if (data === "") return
  if (!data.includes("->")) {
    polymerTemplate = data
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
  let template = polymerTemplate.split('')
  for (let times = 0; times < 10; times+=1) 
    template = applyRules(template)

  const counts = Object.values(count(template))
  return Math.max(...counts) - Math.min(...counts)
}

function partTwo() {
  return "TODO"
}
