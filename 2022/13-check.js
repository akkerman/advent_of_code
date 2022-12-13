module.exports = check

const isInteger = n => typeof n === 'number'
const isList = n => Array.isArray(n)

function check (left, right) {
  if (isInteger(left) && isInteger(right)) {
    if (left < right) return true
    if (left > right) return false
    return undefined
  }

  // both values are arrays
  if (isList(left) && isList(right)) {
    while (left.length > 0 && right.length > 0) {
      const l = left.shift()
      const r = right.shift()
      const result = check(l, r)
      if (result === true || result === false) {
        return result
      }
    }
    if (left.length < right.length) return true
    if (left.length > right.length) return false
    return undefined
  }

  // exactly one value is an integer
  if (isInteger(left) && isList(right)) {
    return check([left], right)
  }
  if (isList(left) && isInteger(right)) {
    return check(left, [right])
  }

  console.log({ left, right })
  throw new Error('huh?')
}
