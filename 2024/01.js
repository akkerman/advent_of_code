const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const R = require('ramda')

const list1 = []
const list2 = []

rl.on('line', data => {
  const [one, two] = data.split('   ')

  list1.push(parseInt(one))
  list2.push(parseInt(two))
})

rl.on('close', () => {
  console.log('partOne', partOne(list1, list2))
  console.log('partTwo', partTwo(list1, list2))
})

function partOne (list1, list2) {
  return R.zip(list1.sort(), list2.sort())
    .map(([a, b]) => Math.abs(a-b))
    .reduce((acc, curr) => acc + curr, 0)
}

function partTwo (list1, list2) {
  return list1.map(item => item * list2.filter(i => i === item).length)
    .reduce((acc, curr) => acc + curr, 0)
}
