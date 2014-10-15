// node benchmark/node.js
//
// http://localhost:5000/

var app = require('http').createServer(handler);

app.listen(5000, "127.0.0.1");

function handler (req, res) {
  res.writeHead(200);
  return res.end("Hello, World");
}
