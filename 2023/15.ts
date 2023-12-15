const R = require('ramda')
const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const sum = (a:number, b:number) => a + b
const int = R.pipe(R.trim, parseInt)


type LabeledLens = string
type Box = string[]
type Step = string
type Instructions = string[]
type FocusingPower = number
type Operation = string

function main () {
  const sequence:string[] = [] 

  rl.on('line', (line: string) => {
    line = line.toString()

    sequence.push(...line.split(','))
  })

  rl.on('close', () => {
    console.log('partOne', partOne(sequence))
    console.log('partTwo', partTwo(sequence))
  })
}

main()

function hash (curr:number, char:string) {
  curr += char.charCodeAt(0)
  curr *= 17
  curr %= 256
  return curr
}

function hashString (step:Step) {
  return step.split('').reduce(hash, 0)
}

function partOne (sequence:Instructions) {
  return sequence
    .map(hashString)
    .reduce(sum)
}

function partTwo (sequence:Instructions): FocusingPower {
  const REMOVE:Operation = '-'
  const REPLACE:Operation = '='
  const boxes:Box[] = Array.from({ length: 256 }, () => [])

  // @ts-ignore - takeWhile handles strings, but type doesn't know...?
  const labelFromStep = R.takeWhile(x => !'-='.includes(x)) as (step:Step)=>string

  const indexOfLens = (box:Box, label:string):number => {
    for (let i = 0; i < box.length; i += 1) {
      if (box[i].includes(label)) return i
    }
    return -1
  }

  const removeLens = (box:Box, index:number):Box => (index === -1)
    ? box
    : R.concat(
      R.take(index, box),
      R.drop(index + 1, box),
    )

  const replaceLens = (box:Box, index:number, lens:LabeledLens):Box => R.concat(
    R.take(index, box),
    R.concat(
      [lens],
      R.drop(index + 1, box),
    ))

  const addLens = (box:Box, lens:LabeledLens):Box => R.concat(
    box, [lens],
  )

  const instruction = (step:Step):Operation => step.includes(REMOVE) ? REMOVE : REPLACE

  const indexedFocusingPower = 
      (acc:FocusingPower, curr:FocusingPower, idx:number) => acc + curr * (idx + 1)

  const boxFocus = (box:Box):FocusingPower => box
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
