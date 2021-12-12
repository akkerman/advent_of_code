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
    if (visited.has(from)) return paths
    if (from === to) {
      paths.push([...currentPath, to])
      return paths
    }
    if (visitOnce.has(from)) visited.add(from)
    currentPath.push(from)
    for (const next of adjacent.get(from)) {
      solve({from:next, to, paths, currentPath, visited})
    }
    currentPath.pop()
    visited.delete(from)
    return paths
  }
}

function findPaths2(from, to, nodeToVisitTwice=null) {
  return solve({from, to})

  function solve({from, to, paths=[], currentPath=[], visited=new Set()}) {
    if (visited.has(from)) return paths
    let label = from
    if (visitOnce.has(label)) {
      if (label === nodeToVisitTwice) {
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
      return paths
    }
    for (const next of adjacent.get(from)) {
      solve({from:next, to, paths, currentPath, visited})
    }
    currentPath.pop()
    visited.delete(label)
    return paths
  }
}



function partOne() {
  const paths = findPaths('start', 'end')

  return paths.length
}

function partTwo() {
  const answ = new Set()
  for (const nodeToVisitTwice  of visitOnce) {
    if (['start','end'].includes(nodeToVisitTwice)) continue
    const paths = findPaths2('start', 'end', nodeToVisitTwice)
    for (let path of paths) {
      answ.add(path.join('-'))
    }
  }

  return answ.size
}
