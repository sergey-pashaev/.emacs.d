#!/bin/bash

mkdir -p build/ && cd build/
CC=clang CXX=clang++ cmake .. # clang
make -j4
cd ../
