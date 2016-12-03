#!/usr/bin/env node

var path = require('path')
var fs = require('fs')
var loc = require('src-location')
// var format = require('standard-format').transform
const standard = require('standard')
const http = require('http')
const querystring = require('querystring')

const errorsign = '#!!#'
const port = 8000
const timeout = 10000
const server = http.createServer((req, res) => {
  let bodyString = ''

  // prevent node died to set a timeout
  req.resume()
  res.setTimeout(timeout, function () {
    console.log('server', 'request timeout')
    res.end('server hello from request timeout')
  })

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
      standard.lintText(result, {fix: true}, (err, out) => {
        console.log(err, result)
        const ret = out.results[0]
        const msg = ret.messages.filter(v => v.fatal).shift()
        if (msg) {
					// convert line+column into zero-based index
          msg.index = loc.locationToIndex(result, msg.line, msg.column) - 1
          console.log(JSON.stringify(msg))
          return res.end(errorsign + JSON.stringify(msg))
        }
        res.end(ret.output)
      })
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
  console.log('Listening on port', port)
})
