#!/bin/bash

################################################################
# Test framework.

function error_exit() {
  echo -n -e "\033[1;31m[ERROR]\033[0;39m "
  echo "$1"
  exit 1
}

function run() {
  echo -n "Testing $1 ... "
  echo "(print $3)" | gosh lisp2js.scm > compiled-result.js
  result=$(cat lisp.js compiled-result.js | node)
  code=$?
  if [ $code -ne 0 ]; then
    error_exit "exit status is not 0 [$code]"
  fi
  if [ "$result" != "$2" ]; then
    error_exit "$2 expected, but got '$result'"
  fi
  rm compiled-result.js
  echo ok
}

################################################################
# Test cases.

run integer 123 '123'
run quote 123 '(quote 123)'
run cons '(1 . 2)' '(cons 1 2)'
run + 6 '(+ 1 2 3)'

################################################################
# All tests succeeded.

echo -n -e "\033[1;32mVM-TEST ALL SUCCEEDED!\033[0;39m\n"
