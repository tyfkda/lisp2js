const fs = require('fs');
function readline() {
  var result = fs.readSync(process.stdin.fd, 100000, null, 'utf-8');
  if (result[1] === 0)
    return null;
  return result[0];
}

var line;
while (line = readline()) {
  process.stdout.write(line);
}

/*
//#!/usr/bin/env node
"use strict";

var fs = require("fs");

var readline = (function() {
  var BUFFER_SIZE = 4096;
  var buffer = new Buffer(BUFFER_SIZE);
  return function() {
    var n = fs.readSync(process.stdin.fd, buffer, 0, BUFFER_SIZE);
    if (n <= 0)
      return null;
    return buffer.slice(0, n).toString();
  };
})();

var line;
while (line = readline()) {
  process.stdout.write(line);
}
*/
