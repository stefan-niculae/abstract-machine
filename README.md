# abstract-machine

Control stack, Work stack, Memory model of execution


## IMP Language

Simple imperative language

### BNF Syntax

```go
Expr ::= nr | Expr iop Expr | !var 
Cond ::= bl | Expr bop Expr
Stmt ::= () | Stmt ; Stmt | var := Expr 
       | if Cond then Stmt else Stmt | while Cond do Stmt
       
nr ::= <int> | <float>
bl ::= true | false
var ::= <string>
iop ::= + | - | * | / | %
bop ::= == | != | < | > | <= | >=
```
