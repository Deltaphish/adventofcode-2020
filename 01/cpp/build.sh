#!/usr/bin/sh
nasm -f elf64 bar.asm
g++ -no-pie bar.o main.cpp -o build/main
./build/main