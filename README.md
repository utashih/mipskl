# mipskl
### Introduction
+ A toy MIPS assembler written in Haskell
+ Using the Tardis monad (see [tardis](https://hackage.haskell.org/package/tardis-0.4.1.0) and this [article](http://kcsongor.github.io/time-travel-in-haskell-for-dummies/) by Csongor Kiss)
+ Designed for the course Computer Organization at Zhejiang University
+ Lots of redundant/inefficient code; refactor in need  
### Usage
Given a file `sample.asm`,
```mips
    add     $t0, $zero, $zero
loop:  
    beq     $a1, $zero, finish
    add     $t0, $t0, $a0
    addi    $a1, $a1, -1
    j       loop
finish: 
    addi    $t0, $t0, 100
    add     $v0, $t0, $zero
```
it generates the following COE file `sample.coe`:
```
./mipskl -a sample.asm sample.coe
```
```
memory_initialization_radix=16; 
memory_initialization_vector=
00004020,
10a00003,
01044020,
20a5ffff,
08000001,
21080064,
01001020;
```
which can then be imported by Xilinx ISE for synthesis.

By disassembling, it can also generates the following 
assembly from the COE file:
```
./mipskl -d sample.coe sample.out.asm
```
```mips
    add     $t0, $zero, $zero
l1: 
    beq     $a1, $zero, l2
    add     $t0, $t0, $a0
    addi    $a1, $a1, -1
    j       l1
l2: 
    addi    $t0, $t0, 100
    add     $v0, $t0, $zero
```

It now supports `add`, `addi`, `addiu`, `addu`, `and`, `andi`, `beq`, `bge`, `bgt`, `ble`, `blt`, `bne`, `j`, `jal`, `jalr`, `jr`, `lb`, `lbu`, `lh`, `lhu`, `lui`, `lw`, `move`, `nor`, `or`, `ori`, `sb`, `sh`, `sll`, `slt`, `slti`, `sltiu`, `sltu`, `sra`, `srl`, `sub`, `subu`, `sw`, `xor`, and `xori`.


---

See also [MIPS.hs](MIPS.hs) (a monadic EDSL for MIPS assembly). 
An example is listed as follows.

```Haskell
program :: MIPS ()          --  example: calculate ($a0) * ($a1) + 100
program = mdo   
    add     t0 zero zero    --          add     $t0, $zero, $zero
    loop <- label           --  loop:  
    beq     a1 zero finish  --          beq     $a1, $zero, finish
    add     t0 t0 a0        --          add     $t0, $t0, $a0
    addi    a1 a1 (-1)      --          addi    $a1, $a1, -1
    j       loop            --          j       loop
    finish <- label         --  finish: 
    addi    t0 t0 100       --          addi    $t0, $t0, 100
    add     v0 t0 zero      --          add     $v0, $t0, $zero
```
which produces the following output
```
   1  00004020
   2  10a00003
   3  01044020
   4  20a5ffff
   5  08000001
   6  21080064
   7  01001020
```


The implementation followed the way introduced in this great [article](http://wall.org/~lewis/2013/10/15/asm-monad.html) by Lewis Wall. 

Currently it only supports `add`, `sub`, `and`, `or`, `addi`, `ori`, `sll`, `srl`, `lw`, `sw`, `lui`, `slt`, `slti`, `beq`, `bne`, `j`, `jal`, and `jr`. 
