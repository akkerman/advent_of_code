function move (pos, steps) {
  return (pos + steps - 1) % 10 + 1
}

function * deterministicDie () {
  let n = 1
  while (true) {
    yield n
    n = n % 100 + 1
  }
}

function * rollTimes (times = 3) {
  let n = 1
  let value = 0

  for (const roll of deterministicDie()) {
    value += roll
    if (n === times) {
      yield value
      n = 0
      value = 0
    }
    n += 1
  }
}
const rollTrice = rollTimes(3)

function partOne () {
  let pos1 = 9// 4
  let pos2 = 10// 8
  let score1 = 0
  let score2 = 0
  let turn = 1

  for (const steps of rollTrice) {
    if (turn % 2 === 0) {
      pos2 = move(pos2, steps)
      score2 += pos2
    } else {
      pos1 = move(pos1, steps)
      score1 += pos1
    }

    if (score1 >= 1000 || score2 >= 1000) break
    turn += 1
  }
  return turn * 3 * Math.min(score1, score2)
}

// possible combinations of throwing a 3-dice trice
// since number of universes is related on dice throw
// take number of combinations into account
const die3Roll = [ // [sum, combinations]
  [3, 1],
  [4, 3],
  [5, 6],
  [6, 7],
  [7, 6],
  [8, 3],
  [9, 1],
]

function play (pos1, pos2, score1, score2, turn, universes) {
  if (score1 >= 21) { return [universes, 0] }
  if (score2 >= 21) { return [0, universes] }
  let w1 = 0
  let w2 = 0
  if (turn % 2 === 0) {
    for (const [steps, combinations] of die3Roll) {
      const pos = move(pos2, steps)
      const [rw1, rw2] = play(pos1, pos, score1, score2 + pos, turn + 1, universes * combinations)
      w1 += rw1
      w2 += rw2
    }
  } else {
    for (const [steps, combinations] of die3Roll) {
      const pos = move(pos1, steps)
      const [rw1, rw2] = play(pos, pos2, score1 + pos, score2, turn + 1, universes * combinations)
      w1 += rw1
      w2 += rw2
    }
  }
  return [w1, w2]
}

function partTwo () {
  const pos1 = 9// 4
  const pos2 = 10// 8
  return Math.max(...play(pos1, pos2, 0, 0, 1, 1))
}

console.log('partOne', partOne()) // 707784
console.log('partTwo', partTwo()) // 157595953724471

// PART 1 - NOTES
// easily done
//
// since I became somewhat facinated with generator functions
// that was used in the solution
//
// Use this alternative in a wile(true) loop
const _deterministicDie = (() => {
  let value = 0
  return () => {
    value = value % 100 + 1
    return value
  }
})()
const _rollTrice = () => _deterministicDie() + _deterministicDie() + _deterministicDie() // eslint-disable-line 

// PART 2 - NOTES
// a few missteps in the thinking process
//
// 1. lets determine all permutations of summing 1,2,3 up to 21 minus the current position
//    there are countless examples on the internet how to do this
//    then lets just use that as a power of 3 (3 possibilities for 1,2,3)
//    WRONG: score is NOT calculated based on the dice, but on position
//
// 2. how to play the game for each possible value of the dice
//    WRONG: score is determined on 3 dice throws
//
// 3. lets generate all solutions based on the sum of the dice
//    GOOD: score is calculated based on next step based on 3 dice throws
//    WRONG: universes split on dice throw not on sum so each sum has has 3 throws
//    times 3 -> still wrong.. ?
//
// 4. current solution
//    realisation struck that there are multiple possible ways to get to the sum of the dice
//    this are the possible sums: 3,4,5,6,7,8,9
//    several of them can be received by different throws, e.g. 5:
//    1,1,3
//    1,2,2
//    1,3,1
//    etc...
//
//    If the solution requires me to calculate all universes where someone wins
//    it will result in an addititional 3*3*3 = 27 possible universes for each existing one
//
//    CAVEAT: these universes exist because the dice created them, not because they are different.
//    CAVEAT2: do not count a throw of 1,1,1 as 3 universes, because there is only 1 die
//             the first of that universe is also in a throw like 1,2,3 and 1,1,2 and 1,3,1.
//
//    - determine the total of universes after a throw (current universes * combination of dice outcome)
//    - pass it along to the next turn
//    - if a player wins return the number of universes that he won in
