#!/usr/bin/env -S guix shell --pure nasm lld execline -- execlineb -S 1
foreground { nasm -felf64 ${1}.asm }
ld.lld -Bstatic -o ${1} ${1}.o
