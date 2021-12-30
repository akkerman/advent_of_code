module.exports = {
  add,
  explode,
  parse,
  split,
  reduce,
}

function parse (number) {
  if (Array.isArray(number)) {
    return new Pair(
      parse(number[0]),
      parse(number[1]),
    )
  }
  return new Regular(number)
}

function add (left, right) {
  return new Pair(left, right)
}

function reduce (pair) {
  while (true) {
    if (explode(pair)) {
      continue
    }
    if (split(pair)) {
      continue
    }
    break
  }
  return pair
}

function explode (pair) {
  const finder = p => p.numParents() === 4 && p instanceof Pair
  const exploding = pair.lfind(finder)
  if (!exploding) return false

  const [left, right] = [exploding.left(), exploding.right()]

  if (right) {
    right.value += exploding.snd.value
  }

  if (left) {
    left.value += exploding.fst.value
  }

  if (exploding.parent.fst === exploding) {
    exploding.parent.fst = new Regular(0)
  } else {
    exploding.parent.snd = new Regular(0)
  }

  return true
}

function split (pair) {
  const finder = p => p instanceof Regular && p.value >= 10
  const splitting = pair.lfind(finder)
  if (!splitting) return false

  const half = splitting.value / 2
  const parsed = parse([Math.floor(half), Math.ceil(half)])

  if (splitting.parent.fst === splitting) {
    splitting.parent.fst = parsed
  } else {
    splitting.parent.snd = parsed
  }
  return true
}

class SnailfishNumber {
  constructor () {
    this.parent = null
  }

  set parent (parent) { this._parent = parent }
  get parent () { return this._parent }
  numParents () {
    if (this.parent === null) return 0
    return 1 + this.parent.numParents()
  }

  right () { return this }
  left () { return this }

  lfind (fn) {
    if (fn(this)) {
      return this
    }
  }

  rfind (fn) {
    if (fn(this)) {
      return this
    }
  }
}

class Pair extends SnailfishNumber {
  constructor (fst, snd) {
    super()
    this.fst = fst
    this.snd = snd
  }

  get fst () { return this._fst }
  set fst (fst) {
    this._fst = fst
    this._fst.parent = this
  }

  get snd () { return this._snd }
  set snd (snd) {
    this._snd = snd
    this._snd.parent = this
  }

  lfind (fn) {
    if (fn(this)) {
      return this
    }
    return this.fst.lfind(fn) || this.snd.lfind(fn)
  }

  rfind (fn) {
    if (fn(this)) {
      return this
    }

    return this.snd.rfind(fn) || this.fst.rfind(fn)
  }

  left () {
    if (this.parent === null) return null
    const p = this.parent

    if (p.snd === this && p.fst instanceof Regular) {
      return p.fst
    }
    if (p.snd === this && p.fst instanceof Pair) {
      return p.fst.rfind(p => p instanceof Regular)
    }
    if (p.fst === this) {
      return p.left()
    }
    return null
  }

  right () {
    if (this.parent === null) return null
    const p = this.parent

    if (p.fst === this && p.snd instanceof Regular) {
      return p.snd
    }
    if (p.fst === this && p.snd instanceof Pair) {
      return p.snd.lfind(p => p instanceof Regular)
    }

    if (p.snd === this) {
      return p.right()
    }
    return null
  }

  show () {
    return [this.fst.show(), this.snd.show()]
  }

  magnitude () {
    return 3 * this.fst.magnitude() + 2 * this.snd.magnitude()
  }
}

class Regular extends SnailfishNumber {
  constructor (value) {
    super()
    this._value = value
    this.left = this
    this.right = this
  }

  set value (value) { this._value = value }
  get value () { return this._value }

  magnitude () {
    return this.value
  }

  show () {
    return this.value
  }
}
