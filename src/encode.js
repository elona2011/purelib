export let codes = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_' //字符串不能改，要做动态替换
let codes2 = 'TmexD9h7kQR0zqrctwSi6MplIuHvA31_L4Pay52nfFEjGgVJNBZ-OdCsYUX8boKW~.'

export const getCodes = () => codes
export const setCodes = (
  newCodes = `0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_`
) => {
  codes = newCodes
}
export const loopMove = s => loop(s, true)
export const unloop = s => loop(s, false)

/**
 * 4位
 * @param time
 */
export const zipTime = time => arrEncode(zipTimeToNum(time))
export const zipTimeD = str => zipTimeToNumD(arrEncodeD(str))

/**
 * 将一串函数代码转换成10位数字(相当于加密函数代码)
 * @param o
 */
export const $fn = o => {
  let oo = ''
  if ((typeof o)['toLowerCase']() === 'function') oo = '' + o
  for (var y = 64222, h = 0; h < o['length']; h++) y ^= (y << 8) + (y >>> 3) + oo['charCodeAt'](h)
  return y
}

/**
 * 字符串循环移位
 * @param s
 */
const loop = (s, right) => {
  //no ast edit
  let r = '',
    codes2Len = codes2.length,
    num = (s.length % 10) + 1

  for (let i = 0; i < s.length; i++) {
    let index = codes2.indexOf(s.charAt(i))
    if (index > -1) {
      let n = index + (right ? num : -num)
      if (n >= codes2Len) n -= codes2Len
      if (n < 0) n += codes2Len
      r += codes2.charAt(n)
    } else {
      throw `error when loop char "${s.charAt(i)}",i=${i},s=${s}`
    }
  }
  return r
}

/**
 * 数组压缩为字符串
 * @param Y
 */
export const Qk = value => {
  if (Array.isArray(value)) {
    for (let i = 0; i < value.length; i++) {
      // (value)[i] = toValidString(value[i])
      value[i] = value[i] + ''
    }
    return value.join('~')
  } else {
    // return toValidString(<string | number>value)
    return value + ''
  }
}

/**
 * 确保只存在65个url valid字符
 * @param s
 */
const toValidString = s => {
  let reg = new RegExp(`[^a-zA-Z0-9._-]+`, 'g')
  return String(s).replace(reg, '_')
}

/**
 * 1字符,target数量压缩，有效值1~
 * @param num
 */
export const zipTargetNum = num => {
  if (num < 1) num = 1

  if (num <= 32) {
    return codes.charAt(num - 1)
  } else if (num <= 64) {
    return codes.charAt(Math.ceil((num - 32) / 2) + 31)
  } else if (num <= 144) {
    return codes.charAt(Math.ceil((num - 64) / 5) + 47)
  } else {
    return codes.charAt(codes.length - 1)
  }
}
export const zipTargetNumD = str => {
  let i = codes.indexOf(str)
  if (i < 32) {
    return i + 1
  } else if (i < 48) {
    return (i - 31) * 2 + 32
  } else {
    return (i - 47) * 5 + 64
  }
}

/**
 * 2字符，位置压缩，包括xy，有效值0~4096*2
 * @param x
 * @param codes
 */
export const zipXY = x => {
  if (x < 0) x = 0
  let x2 = Math.floor(x / 2)

  return zip2Chars(x2)
}
export const zipXYD = str => zip2CharsD(str) * 2

/**
 * 二位字符压缩
 * @param x
 * @param codes
 */
export const zip2Chars = key => {
  if (key < 4096) {
    let r = getParams(key, 2)
    return codes.charAt(r[0]) + codes.charAt(r[1])
  } else {
    return codes.charAt(codes.length - 1) + codes.charAt(codes.length - 1)
  }
}
export const zip2CharsD = str => codes.indexOf(str.charAt(0)) + codes.indexOf(str.charAt(1)) * 64

/**
 * 4字符
 * @param time
 * @param codes
 */
export const zipTimeToNum = time => {
  let codesLen = codes.length,
    len3 = codesLen * codesLen * codesLen,
    step0 = len3 * 30,
    step1 = step0 + len3 * 20 * 10,
    step2 = step1 + len3 * 14 * 50

  if (time < step0) {
    return getParams(time, 4)
  } else if (time < step1) {
    let time10 = Math.floor((time - step0) / 10),
      r = getParams(time10, 4)

    r[r.length - 1] += 30
    return r
  } else if (time < step2) {
    let time50 = Math.floor((time - step1) / 50),
      r = getParams(time50, 4)

    r[r.length - 1] += 50
    return r
  } else {
    return [codesLen - 1, codesLen - 1, codesLen - 1, codesLen - 1]
  }
}
export const zipTimeToNumD = r => {
  let codesLen = codes.length
  if (r[3] < 30) {
    return (
      r[3] * codesLen * codesLen * codesLen + r[2] * codesLen * codesLen + r[1] * codesLen + r[0]
    )
  } else if (r[3] < 50) {
    return (
      7864320 +
      ((r[3] - 30) * codesLen * codesLen * codesLen +
        r[2] * codesLen * codesLen +
        r[1] * codesLen +
        r[0]) *
        10
    )
  } else if (r[3] <= 63) {
    return (
      60293120 +
      ((r[3] - 50) * codesLen * codesLen * codesLen +
        r[2] * codesLen * codesLen +
        r[1] * codesLen +
        r[0]) *
        50
    )
  } else {
    return 243793870
  }
}

/**
 * 计算当前进制下的多项式参数
 * @param num
 */
export const getParams = (num, size) => {
  let r = [],
    codesLen = codes.length

  if (num < 0) num = 0
  while (num >= codesLen) {
    r.push(num % codesLen)
    num = Math.floor(num / codesLen)
  }
  r.push(num)
  return editArrLen(r, size)
}

/**
 * 高位补0
 * @param arr
 */
const editArrLen = (arr, toLen) => {
  while (arr.length < toLen) arr.push(0)
  return arr
}

/**
 * 数据转为codes
 * @param arr
 * @param codes
 */
const arrEncode = arr => {
  let r = ''
  for (let i = 0; i < arr.length; i++) {
    r += codes.charAt(arr[i])
  }
  return r
}
export const arrEncodeD = str => {
  let r = []
  for (let i = 0; i < str.length; i++) {
    r.push(codes.indexOf(str[i]))
  }
  return r
}

export const rrr = s => {
  if (s) {
    var newChars = []
    var offset = s.length % 10

    if (offset == 0) offset++

    for (var i = 0; i < s.length; i++) {
      var code = s.charCodeAt(i)
      code -= offset
      if (code < 0) {
        code += 128
      }
      newChars.push(String.fromCharCode(code))
    }
    return newChars.join('')
  }
  return ''
}
