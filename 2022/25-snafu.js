module.exports = {
  snafu2dec,
  dec2snafu,
}

/**
 * @param {string} str
 * @returns {number}
 */
function snafu2dec (str) {
  let pow = str.length - 1
  let num = 0

  for (const c of str) {
    const base = 5 ** pow

    if (c === '-') {
      num -= base
    } else
    if (c === '=') {
      num -= 2 * base
    } else {
      num += parseInt(c) * base
    }

    pow -= 1
  }
  return num
}

/**
 * @param {number} int
 * @returns {string}
 */
function dec2snafu (int) {
  let snafu = ''
  while (int) {
    let rem = int % 5
    int = Math.floor(int / 5)

    if (rem <= 2) {
      snafu = rem + snafu
    } else {
      if (rem === 3) rem = '='
      if (rem === 4) rem = '-'
      int += 1

      snafu = rem + snafu
    }
  }
  return snafu
}
