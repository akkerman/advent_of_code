/**
 * Split a list in sub lists of given length
 * @template T
 * @param {number} n
 * @param {Array<T>} xs
 * @returns {Array<Array<T>>}
 */
const splitList = (n, xs) => (xs.length)
  ? [xs.slice(0, n)].concat(splitList(n, xs.slice(n)))
  : xs

/**
 * Find elements that all given arrays have in common
 * @template T
 * @param {Array<Array<T>>} arrays
 * @returns {Array<T>}
 */
const findInCommon = arrays => arrays[0].filter(
  item => arrays.slice(1).reduce(
    (acc, curr) => acc && curr.includes(item),
    true,
  ),
)

module.exports = {
  splitList,
  findInCommon,
}
