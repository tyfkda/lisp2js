'use strict'

// For node JS.

if (typeof __non_webpack_require__ === 'undefined')
  global.__non_webpack_require__ = require  // To hack for non-webpacked

const fs = __non_webpack_require__('fs')

const {Stream} = require('./runtime.js')

module.exports = function(LISP) {
  const BUFFER_SIZE = 4096
  class FileStream extends Stream {
    constructor(fd, path) {
      super()
      this.fd = fd
      this.path = path
      this.lines = []
      this.index = 0
      this.buffer = Buffer.alloc(BUFFER_SIZE)
    }

    close() {
      if (this.fd == null)
        return
      fs.closeSync(this.fd)
      this.fd = null
      this.lines.length = this.index = 0
      this.str = null
    }
    // Include '\n' at line end
    readLine() {
      for (;;) {
        let left = ''
        if (this.index < this.lines.length) {
          if (this.index < this.lines.length - 1)  // Not last.
            return this.lines[this.index++]

          left = this.lines[this.index]
          this.lines.length = this.index = 0
        }

        if (this.fd == null)
          return LISP.nil
        const n = fs.readSync(this.fd, this.buffer, 0, BUFFER_SIZE)
        if (n <= 0)
          return left !== '' ? left : null

        let string = left + this.buffer.slice(0, n).toString()
        let start = 0
        for (;;) {
          const pos = string.indexOf('\n', start)
          if (pos < 0) {
            this.lines.push(start === 0 ? string : string.slice(start))
            break
          }
          this.lines.push(string.slice(start, pos + 1))
          start = pos + 1
        }
        this.index = 0
      }
    }
    write(s) {
      fs.writeSync(this.fd, s)
    }
  }

  LISP['*stdin*'] = new FileStream(process.stdin.fd, '*stdin*')
  LISP['*stdout*'] = new FileStream(process.stdout.fd, '*stdout*')
  LISP['*stderr*'] = new FileStream(process.stderr.fd, '*stderr*')

  LISP.open = function open(path, flag) {
    try {
      const fd = fs.openSync(path, flag || 'r')
      return new FileStream(fd, path)
    } catch (e) {
      return LISP.nil
    }
  }

  LISP.close = function close(stream) {
    stream.close()
    return stream
  }

  LISP.load = function load(fileSpec) {
    let stream
    if (typeof fileSpec === 'string') {
      stream = LISP.open(fileSpec)
      if (!stream)
        return LISP.error(`Cannot open [${fileSpec}]`)
    } else if (fileSpec instanceof Stream)
      stream = fileSpec
    else
      return LISP.error(`Illegal fileSpec: ${fileSpec}`)

    if (stream.match(/^#!/, true))
      stream.getLine()  // Skip Shebang.

    let result = LISP.nil
    for (;;) {
      const s = LISP.read(stream)
      if (s === LISP.nil)
        break
      result = LISP.eval(s)
    }
    if (stream !== fileSpec)
      LISP.close(stream)
    return result
  }

  // System
  LISP.exit = function exit(code) { process.exit(code) }

  LISP.jsrequire = __non_webpack_require__
}
