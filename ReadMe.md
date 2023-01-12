# Verilisp
Verilisp is a collection of common lisp macros which spit out verilog HDL code, effectively turning common lisp into a prettier frontend to verilog. 

Original Code is in google code, but it seems it's no longer maintained.

# Issues
## Verilisp is not a compiler
Detecting syntax errors is missing because of just a translator not a compiler.
Execution also depends on an exit simulator e.g. iverilog.

## Non Blocking Assignment(<=)
n= operator is introduced for non blocking assignment
to avoid conflict with less than operator in original verilisp.
Now you can use '<=' lisp function for non blocking assignment
with transport-delay.
If you want to use less than operator, Please use '%<=' or '=<'.

## Expressions about Hex and Bin
Expressions about numeric in Verilog-HDL is very useful. 
You can use in your verilisp program, but it's not elegant.

```
(b 1 1) -> 1'b1
```
