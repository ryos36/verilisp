# Verilisp
Verilisp is a collection of common lisp macros which spit out verilog HDL code, effectively turning common lisp into a prettier frontend to verilog. 

Original Code is in google code, but it seems it's no longer maintained.

# Issues
## Verilisp is not a compiler
Detecting syntax errors is missing because of just a translator not a compiler.
Exection also depends on an exit simulator e.g. iverilog.

## Non Blocking Assignment(<=)
n= operator is introduced n= operator for non blocking assignment
to avoid conflict with less than operator in original verilisp.
I have overwrited <= operator to behave non blocking assignment on 73d4825a.
The new f_<= function has a advantage to be able to use transport-delay, 
but behind the function i.e. less than operator.

## Expressions about Hex and Bin
Expressions about numeric in Verilog-HDL is very useful. 
You can use in verilisp, but it's not elegant.

```
(b 1 1) -> 1'b1
```

I tried to support such expressions. However it's not perfect.