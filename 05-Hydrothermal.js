const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

let maxX = 0
let maxY = 0
const initVentField = (maxX, maxY) => Array.from({length:maxY+1}, () => Array(maxX+1).fill(0))
const lines = []

rl.on("line", data => {
  const line = data.replace(' -> ', ',').split(',').map(s => Number.parseInt(s))
  const [x1,y1,x2,y2] = line

  maxX = Math.max(maxX, x1, x2)
  maxY = Math.max(maxY, y1, y2)

  lines.push(line)

})

rl.on('close', () => {
  console.log('partOne', partOne(lines))
  console.log('partTwo', partTwo(lines))
})

function print(ventField) {
  for (const line of ventField) {
    console.log(line.map(c => c === 0 ? '.' : c).join(''))
  }
}

function partOne(lines) {
  const ventField = initVentField(maxX, maxY)

  for (const [x1,y1,x2,y2] of lines) {
    if (x1===x2) {
      for (let y = Math.min(y1, y2); y <= Math.max(y1, y2); y+=1) {
        ventField[y][x1]+=1
      }
    }

    if (y1===y2) {
      for (let x = Math.min(x1, x2); x <= Math.max(x1, x2); x+=1) {
        ventField[y1][x]+=1
      }
    }
  }


  return ventField.flatMap(arr => arr.filter(n => n >= 2)).length
}

function partTwo(lines) {
  const ventField = initVentField(maxX, maxY)

  for (const [x1,y1,x2,y2] of lines) {
    if (x1===x2) {
      for (let y = Math.min(y1, y2); y <= Math.max(y1, y2); y+=1) {
        ventField[y][x1]+=1
      }
    } else if (y1===y2) {
      for (let x = Math.min(x1, x2); x <= Math.max(x1, x2); x+=1) {
        ventField[y1][x]+=1
      }
    } else {
      let x = x1
      let y = y1

      while (true) {
        ventField[y][x]+=1

        if (x === x2 || y === y2) break

        if (x<x2) x+=1 
        else x-=1

        if (y<y2) y+=1 
        else y-=1
      }
    }
  }

  return ventField.flatMap(arr => arr.filter(n => n >= 2)).length
}
