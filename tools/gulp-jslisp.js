import glutil from 'gulp-util'
import through from 'through2'

// consts
const PLUGIN_NAME = 'gulp-jslisp'

module.exports = (() => {
  const LISP = require('../lisp2js')

  const compile = (codes) => {
    const stream = LISP['make-string-input-stream'](codes)
    const results = []
    for (;;) {
      const s = LISP.read(stream)
      if (s === LISP.nil)
        return results

      const result = LISP.compile(s)
      results.push(`${result};`)
    }
  }

  return () => {
    return through.obj(function(file, encoding, callback) {
      if (file.isNull()) {
        // nothing to do
        return callback(null, file)
      }

      if (file.isStream()) {
        // file.contents is a Stream - https://nodejs.org/api/stream.html
        this.emit('error', new glutil.PluginError(PLUGIN_NAME, 'Streams not supported!'))

        // or, if you can handle Streams:
        //file.contents = file.contents.pipe(...
        //return callback(null, file)
      } else if (file.isBuffer()) {
        const results = compile(file.contents.toString('utf8'))
        file.contents = new Buffer(
          'module.exports = function(LISP) {\n' +
          '  \'use strict\'\n' +
            results.map(s => '  ' + s).join('\n') + '\n' +
          '}\n')
        return callback(null, file)
      }
    })
  }
})()
