import gulp from 'gulp'

import babel from 'gulp-babel'
import uglify from 'gulp-uglify'
import sourcemaps from 'gulp-sourcemaps'
import rename from 'gulp-rename'
import gutil from 'gulp-util'
import eslint from 'gulp-eslint'

import jslisp from './tools/gulp-jslisp'
import embed from './tools/gulp-embed'
import concat from 'gulp-concat'

const destDir = '.'

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
  return gulp.src(kSrcLispFiles)
    .pipe(sourcemaps.init({loadMaps: true}))
    .pipe(concat('lisp2js.js'))
    .pipe(jslisp())
    .pipe(embed({
      template: 'src/runtime/runtime.js',
    }))
    .pipe(babel())
    .pipe(sourcemaps.write('./'))
    .pipe(gulp.dest(destDir))
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
