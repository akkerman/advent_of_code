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

function findPaths(from, to) {
  return solve({from, to})

  function solve({from, to, paths=[], currentPath=[], visited=new Set()}) {
    if (from === to) {
      paths.push([...currentPath, to])
      return paths
    }

    if (visited.has(from)) return paths

    const myVisited = new Set([...visited])

    if (visitOnce.has(from)) myVisited.add(from)

    for (const next of adjacent.get(from)) {
      solve({from:next, to, paths, currentPath:[...currentPath, from], visited:myVisited})
    }
    return paths
  }
}

function findPaths2(from, to) {
  return solve({from, to})

  function solve({from, to, paths=[], currentPath=[], visited=new Set(), hasDouble=false}) {
    if (from === to) {
      paths.push([...currentPath, to])
      return paths
    }

    if (visited.has(from) && (from === 'start' || hasDouble)) return paths

    const myVisited = new Set([...visited])

    if (visitOnce.has(from)) {
      if (myVisited.has(from))
        hasDouble = true
      else
        myVisited.add(from) 
    }

    for (const next of adjacent.get(from)) {
      solve({from:next, to, paths, currentPath:[...currentPath, from], visited:myVisited, hasDouble})
    }
    return paths
  }
}



function partOne() {
  const paths = findPaths('start', 'end')

  return paths.length
}

function partTwo() {
  const paths = findPaths2('start', 'end')

  return paths.length
}
