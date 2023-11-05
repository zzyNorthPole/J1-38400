# J1-CPU

## Description

This repository hosts a single issue machine of MIPS1 isa written in SpinalHDL. Here are some specs:

- MIPS1 instruction set
- pipelined 7 stages
- AXI4 bus
- supported MUL/DIV
- instruction and data caches
- software refilled MMU
- using 2-bit saturating counter as branch predictor
- using store buffer to accelerate uncached store
- parameterized bypass network
- interrupts and exception handling with machine
- Linux compatible

## Build the project

```sh
mill mill.scalalib.GenIdea/idea
```
