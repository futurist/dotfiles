// #!/usr/bin/env node

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
      var result
      // var query = querystring.parse(bodyString)
      // console.log(bodyString)
      try {
        result = format(bodyString || '')
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
