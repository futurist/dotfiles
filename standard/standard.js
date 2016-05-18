// #!/usr/bin/env node

var path = require('path')
var fs = require('fs')
var esprima = require('esprima')
var escodegen = require('escodegen')
var esprimaOptions = {
  raw: true,
  tokens: true,
  range: true,
  comment: true
}
var options = {
  format: {
    indent: {
      style: '    ',
      base: 0,
      adjustMultilineComment: false
    },
    newline: '\n',
    space: ' ',
    json: false,
    renumber: false,
    hexadecimal: false,
    quotes: 'single',
    escapeless: false,
    compact: false,
    parentheses: true,
    semicolons: true,
    safeConcatenation: false
  },
  moz: {
    starlessGenerator: false,
    parenthesizedComprehensionBlock: false,
    comprehensionExpressionStartsWithAssignment: false
  },
  parse: null,
  comment: false,
  sourceMap: undefined,
  sourceMapRoot: null,
  sourceMapWithCode: false,
  file: undefined,
  // sourceContent: originalSource,
  directive: false,
  verbatim: undefined
}

try{
  options = JSON.parse(fs.readFileSync(path.join(__dirname, 'escodegen.json'), 'utf-8'))
}catch(e){}

var format = require('standard-format').transform
var http = require('http')
var querystring = require('querystring')

var errorsign = '#!!#'
var port = 8000
const server = http.createServer((req, res) => {
  var bodyString = ''
  res.setHeader('Content-Type', 'text/html')
  res.writeHead(200, {'Content-Type': 'text/plain'})
  if (req.method == 'POST') {
    req.on('data', function (data) {
      bodyString += data
    })

    req.on('end', function () {
      var result = bodyString || ''
      // var query = querystring.parse(bodyString)
      // console.log(bodyString)
      result = new Buffer(result, 'base64').toString()
      try {

        if (req.url.indexOf('esprima') > -1) {
          // Stage 1 escodegen format
          var syntax = esprima.parse(result, esprimaOptions)
          if (options.comment) {
            escodegen.attachComments(syntax, syntax.comments, syntax.tokens)
          }
          result = escodegen.generate(syntax, options)
        }

        // Stage 2 standard format
        result = format(result)

        // Stage 3 make newline before /** */ comment
        result = result.replace(/\/\*\*/g, '\n$&')

      } catch(e) {
        console.log(JSON.stringify(e))
        result = errorsign + JSON.stringify(e)
      }
      res.end(result)
    })
  }
  if (req.method == 'GET') {
    console.log(req.url)
    switch (req.url) {
    case '/exit':
      console.log('exit now')
      res.end('')
      process.exit(0)
      break
    case '/getport':
      res.end(port + '')
      break
    }
  }
}).listen(port, function () {
  console.log('Listening on port ' + port)
})
