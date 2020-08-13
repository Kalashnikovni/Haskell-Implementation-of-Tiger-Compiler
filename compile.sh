#!/bin/bash

search_dir="/home/kalashnikov/Materias/2017/Compiladores/Haskell-Implementation-of-Tiger-Compiler/test/test_code/good"
out_dir="/home/kalashnikov/Materias/2017/Compiladores/Haskell-Implementation-of-Tiger-Compiler/output"
obj_dir="/home/kalashnikov/Materias/2017/Compiladores/MIPS/Objects"
exec_dir="${out_dir}/exec"
runtime="${out_dir}/runtime.c"

# Run HaskTiger
for entry in "$search_dir"/*
do
  echo "$entry"
  stack exec -- HaskTiger $entry
done
