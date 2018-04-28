#!/usr/bin/env bash

prefix="resources/"

if [ "$#" -ne 1 ]; then
  stack run -- -- -d "${prefix}test.tex"
else
  stack run -- -- -d "${prefix}$1"
fi
