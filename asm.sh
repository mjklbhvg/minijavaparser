#!/usr/bin/env -S guix shell --pure nasm binutils coreutils dash -- dash
set -xe
nasm -felf64 $1.asm
ld --no-dynamic-linker --output $1 $1.o
rm $1.o
