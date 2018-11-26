#!/bin/bash

LISP_RUNNER="../jslisp"

if [ $# -eq 1 ]; then
    LISP_RUNNER=$1
fi

################################################################
# Test framework.

function error_exit() {
  echo -n -e "\033[1;31m[ERROR]\033[0;39m "
  echo "$1"
  exit 1
}

function run() {
  echo -n "Testing $1 ... "
  result=$($LISP_RUNNER ${@:3})
  code=$?
  if [ $code -ne 0 ]; then
    error_exit "exit status is not 0 [$code]"
  fi
  if [ "$result" != "$2" ]; then
    error_exit "'$2' expected, but got '$result'"
  fi
  echo ok
}

################################################################
# Test cases.

run 'hello' 'Hello, world!' ../examples/hello.lisp
run 'fib' 55 '../examples/fib.lisp 10'
run 'echo' 'foo bar baz' ../examples/echo.lisp foo bar baz

content=`cat ../examples/fib.lisp`
run 'cat' "$content" '../examples/cat.lisp' ../examples/fib.lisp


################################################################
# All tests succeeded.

echo -n -e "\033[1;32mTEST ALL SUCCEEDED!\033[0;39m\n"
