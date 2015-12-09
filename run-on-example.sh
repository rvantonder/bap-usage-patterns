#!/bin/bash

if [ "$#" -ne 1 ]
then
  printf "Select an example file to run:\n"
  printf "Usage: ./run-on-example [ ex1-1.ml | ex1-2.ml ... ]\n"
  exit 1
fi

case $1 in
  ex5-*.ml)
  ocaml examples/$1 test/fib-arm;;
  ex*)
  ocaml examples/$1 test/example;;
  *)
  printf "Invalid example, try again.\n"
esac
