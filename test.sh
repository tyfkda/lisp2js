#!/bin/bash

################################################################
# Test framework.

function error_exit() {
  echo -n -e "\033[1;31m[ERROR]\033[0;39m "
  echo "$1"
  exit 1
}

function run() {
  run_raw "$1" "$2" "(print $3)"
}

function run_raw() {
  echo -n "Testing $1 ... "
  result=$(echo "$3" | ./runlisp)
  code=$?
  if [ $code -ne 0 ]; then
    error_exit "exit status is not 0 [$code]"
  fi
  if [ "$result" != "$2" ]; then
    error_exit "$2 expected, but got '$result'"
  fi
  echo ok
}

################################################################
# Test cases.

run integer 123 '123'
run symbol abc '(quote abc)'
run string abc '"abc"'
run t 't' 't'
run nil 'nil' 'nil'
run quote 123 '(quote 123)'
run quote '(1 nil 3)' '(quote (1 () 3))'
run cons '(1 . 2)' '(cons 1 2)'
run car 1 "(car '(1 . 2))"
run cdr 2 "(cdr '(1 . 2))"
run if-true 2 "(if 1 2 3)"
run if-false 3 "(if nil 2 3)"
run if-false2 nil "(if nil 2)"
run_raw set! 123 "(define xyz nil) (set! xyz 123) (print xyz)"
run begin 3 '(begin 1 2 3)'
run begin-empty nil '(begin)'
run lambda 2222 '((lambda (x) (+ x x)) 1111)'
run nested-lambda 3 '(((lambda (x) (lambda (y) (+ x y))) 1) 2)'
run lambda-rest '(1 2 3)' '((lambda (x . y) (cons x y)) 1 2 3)'
run lambda-rest2 '(1)' '((lambda (x . y) (cons x y)) 1)'
run_raw define 123 "(define x 123)
                    (print x)"
run_raw define-lambda 2222 "(define (double x) (+ x x))
                            (print (double 1111))"
run + 6 '(+ 1 2 3)'

# Macros.
run_raw defmacro nil "(define-macro nil! (x) (list 'define x 'nil))
                      (nil! xyz)
                      (print xyz)"

################################################################
# All tests succeeded.

echo -n -e "\033[1;32mTEST ALL SUCCEEDED!\033[0;39m\n"
