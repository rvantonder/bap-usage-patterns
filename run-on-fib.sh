#!/bin/bash

echo "Output for ARM:"
ocaml ex5-2.ml test/fib-arm
echo "====================="

echo "Output for x86:"
ocaml ex5-2.ml test/fib-x86
echo "====================="

echo "Output for x64:"
ocaml ex5-2.ml test/fib-x64
echo "====================="
