const readline = require('readline')
const rl = readline.createInterface({ input: process.stdin })

const nodes = new Set()
const graph = []

const adjacent = new Map()

const visitOnce = new Set()

function addEdge(a,b) {
  if (!adjacent.has(a)) {
    adjacent.set(a,new Set())
  }
  if(!adjacent.has(b)) {
    adjacent.set(b,new Set())
  }

  adjacent.get(a).add(b)
  adjacent.get(b).add(a)

  if (a.toLowerCase() === a)
    visitOnce.add(a)
  if (b.toLowerCase() === b)
    visitOnce.add(b)
}

rl.on("line", data => {
  const edge = data.split('-')
  addEdge(...edge)
  nodes.add(edge[0])
  nodes.add(edge[1])
})

rl.on('close', () => {
  console.log(adjacent)
  console.log(visitOnce)
  
  console.log('partOne', partOne(graph))
  console.log('partTwo', partTwo(graph))
})


function findPaths(from, to, visited, paths, currentPath) {
  if (visited.has(from)) return
  if (visitOnce.has(from)) visited.add(from)
  currentPath.push(from)
  if (from === to) {
    paths.push([...currentPath])
    visited.delete(from)
    currentPath.pop()
    return
  }
  for (const next of adjacent.get(from)) {
    findPaths(next, to, visited, paths, currentPath)
  }
  currentPath.pop()
  visited.delete(from)
}

function findPaths2(from, to, visited, paths, currentPath, visitTwice) {
  if (visited.has(from)) return
  let label = from
  if (visitOnce.has(label)) {
    if (label === visitTwice) {
      if (!visited.has(label+"1")) 
        label = label+"1"
    } 

    visited.add(label)
  } 
  currentPath.push(from)
  if (from === to) {
    paths.push([...currentPath])
    visited.delete(label)
    currentPath.pop()
    return
  }
  for (const next of adjacent.get(from)) {
    findPaths2(next, to, visited, paths, currentPath, visitTwice)
  }
  currentPath.pop()
  visited.delete(label)
}



function partOne() {
  const visited = new Set()
  const paths = []
  findPaths('start', 'end', visited, paths, [])

  return paths.length
}

function partTwo() {
  const answ = new Set()
  for (const n of visitOnce) {
    if (['start','end'].includes(n))continue
    const visited = new Set()
    const paths = []
    findPaths2('start', 'end', visited, paths, [], n)
    for (let path of paths) {
      answ.add(path.join('-'))
    }
  }

  return answ.size
}
