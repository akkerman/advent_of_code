const { partOne } = require('./16.js')
describe('version sum', function() {
  test.each([
    ['8A004A801A8002F478', 16],
    ['620080001611562C8802118E34', 12],
    ['C0015000016115A2E0802F182340', 23],
    ['A0016C880162017C3686B18A3D4780', 31],
  ])('correct sum', (message, sum) => {
    expect(partOne(message)).toEqual(sum)
  });
});
