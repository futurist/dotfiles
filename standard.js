#!/usr/bin/env node

var format = require('standard-format').transform
var http = require('http')
var querystring = require('querystring')

var port = 80
const server = http.createServer((req, res) => {
  var bodyString = ''
  res.setHeader('Content-Type', 'text/html')
  res.writeHead(200, {'Content-Type': 'text/plain'})
  if (req.method == 'POST') {
    req.on('data', function (data) {
      bodyString += data
    })

    req.on('end', function () {
      var query = querystring.parse(bodyString)
      res.end(format(query.text||''))
    })
  }
  if(req.method=='GET') {
    res.end()
    console.log(req.url)
    if(req.url=='/exit'){
      console.log('exit now')
      process.exit(0)
    }
  }
}).listen(port, function(){
  console.log('Listening on port ' + port)
})
