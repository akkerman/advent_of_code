const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })
const boards = []
let drawSequence

rl.on("line", data => {
  if (data === '')
    boards.push([])
  else
    if (data.includes(' '))
      boards[boards.length-1].push(data.trim('').split(/ +/).map(v=>Number.parseInt(v)))
    else 
    drawSequence = data.split(',').map(v=>Number.parseInt(v))
})

rl.on('close', () => {

  const result = play(drawSequence, boards)
  console.log('partOne', partOne(result))
  console.log('partTwo', partTwo(result))
})


function play(drawSequence, boards) {
  return boards.map(playOne)

  function playOne(board) {
    function mark(draw) {
      for (let row of board) {
        const idx = row.indexOf(draw)
        if (idx !== -1)
          row[idx] = 'X'
      }
    }

    const isAWin = nums => nums.reduce((w, c) => w&& c==='X', true)
    function isWinningBoard() {
      for (const row of board) {
        if (isAWin(row)) return true
      }
      for (let i=0; i<board.length;i+=1) {
        const column = board.map(row => row[i])
        if (isAWin(column)) 
          return true
      }
      return false
    }

    for (let numDraws=0; numDraws < drawSequence.length; numDraws+=1) {
      const draw = drawSequence[numDraws]
      mark(draw)
      if (isWinningBoard()) {
        return { board, numDraws, draw }
      }
    }
  }
}

const score = winning => winning.board.flatMap(_=>_).filter(c=>c!=='X').reduce((a,b)=>a+b) * winning.draw

function partOne(result) {
  const winning = result.reduce(
    (winning, current) => winning.numDraws < current.numDraws ? winning : current,
    {numDraws: Number.MAX_SAFE_INTEGER}
  )

  return score(winning)
}

function partTwo(result) {

  const winning = result.reduce(
    (winning, current) => winning.numDraws > current.numDraws ? winning : current,
    {numDraws: 0}
  )

  return score(winning)
}
