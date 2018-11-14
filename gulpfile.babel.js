import gulp from 'gulp'

import babel from 'gulp-babel'
import uglify from 'gulp-uglify'
import sourcemaps from 'gulp-sourcemaps'
import rename from 'gulp-rename'
import gutil from 'gulp-util'
import eslint from 'gulp-eslint'

import jslisp from './tools/gulp-jslisp'

import webpack from 'webpack'
import webpackStream from 'webpack-stream'
import {webpackJsLispConfig, webpackLisp2JsConfig} from './webpack.config.babel'

import plumber from 'gulp-plumber'
import gulpFunction from 'gulp-function'

import fs from 'fs'
import path from 'path'
import util from 'util'

const destDir = '.'
const GEN_DIR = 'gen'
const DIST_DIR = 'dist'

const kSrcLispFiles = [
  'src/basic.lisp',
  'src/backquote.lisp',
  'src/parser.lisp',
  'src/compiler.lisp',
  'src/version.lisp',
]
const kRuntimeFiles = [
  'src/runtime/*.js',
]

gulp.task('build', () => {
  return gulp.src(kSrcLispFiles, {base: 'src'})
    .pipe(jslisp())
    .pipe(rename({extname: '.js'}))
    .pipe(gulp.dest(GEN_DIR))
})

gulp.task('lint', () => {
  return gulp.src(['src/**/*.js',
                   'test/**/*.js',
                   'gulpfile.babel.js'])
    .pipe(eslint())
    .pipe(eslint.format())
    .pipe(eslint.failOnError())
})

gulp.task('watch', () => {
  gulp.watch(kSrcLispFiles.concat(kRuntimeFiles),
             gulp.series('build'))
})

gulp.task('default', gulp.series('build', 'watch'))




gulp.task('pack-jslisp', () => {
  // It looks sourcemap isn't generated using rename, so do it by manually.

  const config = webpackJsLispConfig
  return gulp.src('./src/runtime/jslisp.js')
    .pipe(plumber())
    //.pipe(rename('jslisp'))
    .pipe(webpackStream(config, webpack))
    .pipe(gulp.dest(DIST_DIR))
    .pipe(gulpFunction(async (file, enc) => {
      if (path.basename(file.path) === 'jslisp.js') {
        const promisify = util.promisify
        const content = await promisify(fs.readFile)('./dist/jslisp.js')
        const fd = await promisify(fs.open)('dist/jslisp', 'w')
        await promisify(fs.write)(fd, '#!/usr/bin/env node\n')
        await promisify(fs.write)(fd, 'var __module = module;')
        await promisify(fs.write)(fd, content)
        const stat = await promisify(fs.fstat)(fd)
        await promisify(fs.ftruncate)(fd, stat.size)
        await promisify(fs.close)(fd)
        await promisify(fs.chmod)('dist/jslisp', 0o755)
        await promisify(fs.unlink)('dist/jslisp.js')
      }
    }))
})

gulp.task('pack-lisp2js', () => {
  const config = webpackLisp2JsConfig
  return gulp.src('./src/runtime/lisp2js.js')
    .pipe(plumber())
    .pipe(webpackStream(config, webpack))
    .pipe(gulp.dest(DIST_DIR))
})

gulp.task('release', gulp.series('build', gulp.parallel('pack-jslisp', 'pack-lisp2js')))
