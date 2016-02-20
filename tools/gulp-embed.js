import glutil from 'gulp-util'
import through from 'through2'

import fs from 'fs'

// consts
const PLUGIN_NAME = 'gulp-embed'

module.exports = (() => {
  return (opts) => {
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
        const template = fs.readFileSync(opts.template).toString('utf8')
        const contents = file.contents
        const results = []
        template.split('\n').forEach(line => {
          results.push(line)
          if (opts.pattern.test(line))
            results.push(contents)
        })
        file.contents = new Buffer(results.join('\n'))
        return callback(null, file)
      }
    })
  }
})()
