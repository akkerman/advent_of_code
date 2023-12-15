const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const sum = (a, b) => a + b
const int = R.pipe(R.trim, parseInt)

function main () {
  /** @type string[] */
  const sequence = []

  rl.on('line', line => {
    line = line.toString()

    sequence.push(...line.split(','))
  })

  rl.on('close', () => {
    console.log('partOne', partOne(sequence))
    console.log('partTwo', partTwo(sequence))
  })
}

main()

/** @type {(curr:number, char:string) => number} */
function hash (curr, char) {
  curr += char.charCodeAt(0)
  curr *= 17
  curr %= 256
  return curr
}

/** @type {(step:string) => number} */
function hashString (step) {
  return step.split('').reduce(hash, 0)
}

/** @type {(sequence:string[]) => number} */
function partOne (sequence) {
  return sequence
    .map(hashString)
    .reduce(sum)
}

/**
 * @typedef {string} LabeledLens
 * @typedef {LabeledLens[]} Box
 * @typedef {string} Step
 * @typedef {Step[]} Instructions
 * @typedef {number} FocusingPower
 * @typedef {string} Operation
 */

/** @type {(sequence:Instructions) => FocusingPower} */
function partTwo (sequence) {
  /** @type {Operation} */
  const REMOVE = '-'
  /** @type {Operation} */
  const REPLACE = '='
  /** @type {Box[]} */
  const boxes = Array.from({ length: 256 }, () => [])

  /** @type {(step:Step) => string} */
  const labelFromStep = R.takeWhile(x => !'-='.includes(x))

  /** @type {(box:Box, label:string) => number} */
  const indexOfLens = (box, label) => {
    for (let i = 0; i < box.length; i += 1) {
      if (box[i].includes(label)) return i
    }
    return -1
  }

  /** @type {(box:Box, index:number) => Box} */
  const removeLens = (box, index) => (index === -1)
    ? box
    : R.concat(
      R.take(index, box),
      R.drop(index + 1, box),
    )

  /** @type {(box:Box, index:number, lens:LabeledLens) => Box} */
  const replaceLens = (box, index, lens) => R.concat(
    R.take(index, box),
    R.concat(
      [lens],
      R.drop(index + 1, box),
    ))

  /** @type {(box:Box, lens:LabeledLens) => Box} */
  const addLens = (box, lens) => R.concat(
    box, [lens],
  )

  /** @type {(step:string) => REMOVE|REPLACE} */
  const instruction = step => step.includes(REMOVE) ? REMOVE : REPLACE

  /** @type {(acc:FocusingPower, curr:FocusingPower, idx:number) => FocusingPower} */
  const indexedFocusingPower = (acc, curr, idx) => acc + curr * (idx + 1)

  /** @type {(box: Box) => FocusingPower} */
  const boxFocus = box => box
    .map(l => int(l.split('=')[1]))
    .reduce(indexedFocusingPower, 0)

  for (const step of sequence) {
    const label = labelFromStep(step)
    const boxId = hashString(label)
    const box = boxes[boxId]
    const lensId = indexOfLens(box, label)

    switch (instruction(step)) {
      case REMOVE:
        if (lensId > -1) {
          boxes[boxId] = removeLens(box, lensId)
        }
        break
      case REPLACE:
        if (lensId > -1) {
          boxes[boxId] = replaceLens(box, lensId, step)
        } else {
          boxes[boxId] = addLens(box, step)
        }
        break
    }
  }

  return boxes.map(boxFocus).reduce(indexedFocusingPower, 0)
}
