#! /bin/sh

build/rmatch > build/dynbin
objdump -M x86-64,intel-mnemonic -D -b binary -m i386 build/dynbin --no-show-raw-insn
