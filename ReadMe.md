# Verilisp
Verilisp is a collection of common lisp macros which spit out verilog HDL code, effectively turning common lisp into a prettier frontend to verilog. 

Original Code is in google code, but it seems it's no longer maintained.

# Issues
## Verilisp is not a compiler
Detecting syntax errors is missing because of just a translator not a compiler.
Execution also depends on an exit simulator e.g. iverilog.

## Non Blocking Assignment(n= <=#)
n= operator is introduced for non blocking assignment
to avoid conflict with less than or equal operator in original verilisp.
Now you can use '<=#' lisp function for non blocking assignment
with transport-delay.

## Less Than or equal operator Assignment(<=)
<= is a less than operator not assignment operator.
Also you can use '%<=' or '=<' as less than or equal operator, but it's experimental.

## Expressions about Hex and Bin
Expressions about numeric in Verilog-HDL is very useful. 
You can use in your verilisp program, but it's not elegant.

```
(b 1 1) -> 1'b1
```
