import gulp from 'gulp'

import babel from 'gulp-babel'
import uglify from 'gulp-uglify'
import sourcemaps from 'gulp-sourcemaps'
import rename from 'gulp-rename'
import gutil from 'gulp-util'
import eslint from 'gulp-eslint'

const destDir = '.'

gulp.task('default', [], () => {
  console.log('dummy')
})

gulp.task('babel', () => {
  gulp.src('./runtime.js')
    .pipe(sourcemaps.init({loadMaps: true}))
    .pipe(babel())
    .pipe(rename('lisp2js.js'))
    .pipe(sourcemaps.write('./'))
    .pipe(gulp.dest(destDir))
})

gulp.task('uglify', () => {
  gulp.src('./lisp2js.js')
    .pipe(sourcemaps.init({loadMaps: true}))
    .pipe(babel())
    .pipe(uglify().on('error', gutil.log))
    .pipe(rename('lisp2js.min.js'))
    .pipe(sourcemaps.write('./'))
    .pipe(gulp.dest(destDir))
})

gulp.task('lint', () => {
  return gulp.src(['src/**/*.js',
                   'test/**/*.js',
                   'gulpfile.babel.js'])
    .pipe(eslint())
    .pipe(eslint.format())
    .pipe(eslint.failOnError())
})
