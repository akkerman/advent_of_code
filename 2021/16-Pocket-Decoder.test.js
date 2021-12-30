const { partOne, partTwo, literal, hex2message, operator } = require('./16-Pocket-Decoder.js')

describe('hex2message', function () {
  it.each([
    ['D2FE28', '110100101111111000101000'],
    ['38006F45291200', '00111000000000000110111101000101001010010001001000000000']
  ])('convert %s to binary string', (input, expected) => {
    expect(hex2message(input)).toEqual(expected)
  }
  )
})

describe('literal', () => {
  test.each([
    ['110100101111111000101000', 2021, '000'],
    ['11010001010', 10, ''],
    ['11010010001001000000000', 20, '0000000'],
    ['1101000101001010010001001000000000', 10, '01010010001001000000000']
  ])('parse literal %s', (input, value, rest) => {
    const expectedResult = [{ version: 6, typeId: 4, value }, rest]
    expect(literal(input)).toEqual(expectedResult)
  })
})

describe('operator with length id 0', function () {
  test('parse operator 38006F45291200 to 10 and 20', () => {
    const input = '38006F45291200'
    const message = hex2message(input)
    expect(operator(message)[0]).toEqual({
      version: 1,
      typeId: 6,
      lengthTypeId: '0',
      packets: [
        { version: 6, typeId: 4, value: 10 },
        { version: 2, typeId: 4, value: 20 }
      ]
    })
  })
})
describe('operator with length id 1', function () {
  test('parse operator EE00D40C823060 to 1 2 3', () => {
    const input = 'EE00D40C823060'
    const message = hex2message(input)
    expect(operator(message)[0]).toEqual({
      version: 7,
      typeId: 3,
      lengthTypeId: '1',
      packets: [
        { version: 2, typeId: 4, value: 1 },
        { version: 4, typeId: 4, value: 2 },
        { version: 1, typeId: 4, value: 3 }
      ]
    })
  })
})

describe('nested operators', function () {
  test('should work', () => {
    const input = '8A004A801A8002F478'
    const message = hex2message(input)
    const result = operator(message)
    expect(result[0]).toEqual({
      version: 4,
      typeId: 2,
      lengthTypeId: '1',
      packets: [{
        version: 1,
        typeId: 2,
        lengthTypeId: '1',
        packets: [{
          version: 5,
          typeId: 2,
          lengthTypeId: '0',
          packets: [{
            version: 6,
            typeId: 4,
            value: 15
          }]
        }]
      }]
    })
  })

  test('should work again', () => {
    const input = '620080001611562C8802118E34'
    const message = hex2message(input)
    const result = operator(message)
    expect(result[0]).toEqual({
      version: 3,
      typeId: 0,
      lengthTypeId: '1',
      packets: [{
        lengthTypeId: '0',
        version: 0,
        typeId: 0,
        packets: [
          { version: 0, typeId: 4, value: 10 },
          { version: 5, typeId: 4, value: 11 }
        ]
      }, {
        version: 1,
        typeId: 0,
        lengthTypeId: '1',
        packets: [
          { version: 0, typeId: 4, value: 12 },
          { version: 3, typeId: 4, value: 13 }
        ]
      }]
    })
  })
})

const myInput = 'E0525D9802FA00B80021B13E2D4260004321DC648D729DD67B2412009966D76C0159ED274F6921402E9FD4AC1B0F652CD339D7B82240083C9A54E819802B369DC0082CF90CF9280081727DAF41E6A5C1B9B8E41A4F31A4EF67E2009834015986F9ABE41E7D6080213931CB004270DE5DD4C010E00D50401B8A708E3F80021F0BE0A43D9E460007E62ACEE7F9FB4491BC2260090A573A876B1BC4D679BA7A642401434937C911CD984910490CCFC27CC7EE686009CFC57EC0149CEFE4D135A0C200C0F401298BCF265377F79C279F540279ACCE5A820CB044B62299291C0198025401AA00021D1822BC5C100763A4698FB350E6184C00A9820200FAF00244998F67D59998F67D5A93ECB0D6E0164D709A47F5AEB6612D1B1AC788846008780252555097F51F263A1CA00C4D0946B92669EE47315060081206C96208B0B2610E7B389737F3E2006D66C1A1D4ABEC3E1003A3B0805D337C2F4FA5CD83CE7DA67A304E9BEEF32DCEF08A400020B1967FC2660084BC77BAC3F847B004E6CA26CA140095003900BAA3002140087003D40080022E8C00870039400E1002D400F10038C00D100218038F400B6100229500226699FEB9F9B098021A00800021507627C321006E24C5784B160C014A0054A64E64BB5459DE821803324093AEB3254600B4BF75C50D0046562F72B1793004667B6E78EFC0139FD534733409232D7742E402850803F1FA3143D00042226C4A8B800084C528FD1527E98D5EB45C6003FE7F7FCBA000A1E600FC5A8311F08010983F0BA0890021F1B61CC4620140EC010100762DC4C8720008641E89F0866259AF460C015D00564F71ED2935993A539C0F9AA6B0786008D80233514594F43CDD31F585005A25C3430047401194EA649E87E0CA801D320D2971C95CAA380393AF131F94F9E0499A775460'
describe('version sum', function () {
  test.each([
    [16, '8A004A801A8002F478'],
    [12, '620080001611562C8802118E34'],
    [23, 'C0015000016115A2E0802F182340'],
    [31, 'A0016C880162017C3686B18A3D4780']

  ])('correct sum %s', (sum, hex) => {
    expect(partOne(hex)).toEqual(sum)
  })

  it('should give me the answer to part one', () => {
    expect(partOne(myInput)).toEqual(940)
  })
})

describe('calculate', function () {
  test.each([
    [3, 'C200B40A82'],
    [54, '04005AC33890'],
    [7, '880086C3E88112'],
    [9, 'CE00C43D881120'],
    [1, 'D8005AC2A8F0'],
    [0, 'F600BC2D8F'],
    [0, '9C005AC2F8F0'],
    [1, '9C0141080250320F1802104A08']
  ])('correct calculation %s', (result, hex) => {
    expect(partTwo(hex)).toEqual(result)
  })
  it('should give me the answer to part two', () => {
    expect(partTwo(myInput)).toEqual(13476220616073)
  })
})
