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

const destDir = '.'
const GEN_DIR = 'gen'
const DIST_DIR = 'dist'

const kSrcLispFiles = [
  'src/basic.lisp',
  'src/backquote.lisp',
  'src/parser.lisp',
  'src/compiler.lisp',
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

gulp.task('release', gulp.series('build', () => {
  return gulp.src('./lisp2js.js')
    .pipe(sourcemaps.init({loadMaps: true}))
    .pipe(babel())
    .pipe(uglify().on('error', gutil.log))
    .pipe(rename('lisp2js.min.js'))
    .pipe(sourcemaps.write('./'))
    .pipe(gulp.dest(destDir))
}))

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
  const config = webpackJsLispConfig
  //delete config.output.sourceMapFilename
  return gulp.src('./src/runtime/jslisp.js')
    .pipe(plumber())
    .pipe(webpackStream(config, webpack))
    .pipe(rename('jslisp'))
    .pipe(gulp.dest(DIST_DIR))
})

gulp.task('pack-lisp2js', () => {
  const config = webpackLisp2JsConfig
  return gulp.src('./src/runtime/lisp2js.js')
    .pipe(plumber())
    .pipe(webpackStream(config, webpack))
    .pipe(rename('lisp2js.js'))
    .pipe(gulp.dest(DIST_DIR))
})

gulp.task('pack', gulp.parallel('pack-jslisp', 'pack-lisp2js'))
