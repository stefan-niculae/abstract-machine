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

**Brackets and parentheses**:
```javascript
while x > y do {
  x := (5+1) / 2;
}
```

- not needed for `if`/`while` condition
- optional for `if`/`while` bodies
- optional in expressions


**Order of Operations**:

- Bug: `4 + 6/2` evaluates to `(4+6) / 2`
- Workaround: `4 + (6/2)` to get `7`


**Keywords**:

- Bug: the following are treated as a variable name: `true, false, if, then, else, while, do`


### Operational Semantics

Configuration: Control, Stack, Memory `<c, s, m>`

Rules:

```php
# Arithmetic Expressions
          <n c, s, m>  ->  <c, n s, m>
         <!v c, s, m>  ->  <c, m(v) s, m>
<(E1 iop E2) c, s, m>  ->  <E1 E2 iop c, s, m>
  <iop c, n2 n1 s, m>  ->  <c, n s, m> where n = n1 `iop` n2

# Boolean Conditions
          <b c, s, m>  ->  <b, n s, m>
<(E1 bop E2) c, s, m>  ->  <E1 E2 bop c, s, m>
  <bop c, n2 n1 s, m>  ->  <c, b s, m> where b = n1 `bop` n2

# Statements
       <() c, s, m>  ->  <c, s, m>
<(S1 ; S2) c, s, m>  ->  <S1 S2 c, s, m>
     <v:=E c, s, m>  ->  <E := c, v s, m>
   <:= c, n v s, m>  ->  <c, s, m[v=n]>
                
# Branch
<(if C then St else Sf) c, s, m>  ->  <C branch c, St Sf s, m>
  <branch c, /true/  St Sf s, m>  ->  <St c, s, m>
  <branch c, /false/ St Sf s, m>  ->  <Sf c, s, m>

# Loop
  <(while C do S) c, s, m>  -> <C loop c, C S s, m>
<loop c, /false/ C S s, m>  ->  <c, s, m>
<loop c, /true/  C S s, m>  ->  <S (while C do S) c, s, m>
```
