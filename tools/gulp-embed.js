import glutil from 'gulp-util'
import through from 'through2'
import ejs from 'ejs'

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

        const data = {
          contents
        }
        const options = null
        const rendered = ejs.render(template, data, options)

        file.contents = new Buffer(rendered)
        return callback(null, file)
      }
    })
  }
})()
