const cjstoes = require('cjstoes')
const fse = require('fs-extra')
const glob = require('glob')
const { readFileSync, writeFileSync } = require('fs')

fse.copySync(`${process.cwd()}/output`, `${process.cwd()}/outputES6`)
glob.sync(`${process.cwd()}/outputES6/**/*.js`).forEach(file => {
  let f = readFileSync(file, 'utf8')
  let nf = cjstoes(f, file)
  writeFileSync(file, nf)
})
