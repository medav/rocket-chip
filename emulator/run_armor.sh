#!/bin/bash

./emulator-freechips.rocketchip.system-DefaultConfig-debug +max-cycles=1000000 +verbose ../../riscv-tools/riscv-tests/benchmarks/armor.riscv 3>&1 1>&2 2>&3 | /mnt/c/Users/Michael/Documents/riscv/bin/spike-dasm  > output/armor.riscv.out && [ $PIPESTATUS -eq 0 ]
