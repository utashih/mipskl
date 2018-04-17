# mipskl

A toy MIPS assembler written in Haskell

So far please see [MIPS.hs](MIPS.hs) (not an assembler yet). An examle is listed as follows.

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
