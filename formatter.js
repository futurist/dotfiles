var format = require('standard-format').transform
var http = require('http')
const server = http.createServer((req, res) => {
  var bodyString = ''
  res.setHeader('Content-Type', 'text/html')
  res.writeHead(200, {'Content-Type': 'text/plain'})
  if (req.method == 'POST') {
    req.on('data', function (data) {
      bodyString += data
    })

    req.on('end', function () {
      res.end(format(bodyString))
    })
  } else {
    res.end('ok' + req.method + req.url)
  }
}).listen(80)
