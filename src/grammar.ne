@preprocessor coffee
@{% %} # fix for coffee preprocessor...

@{% {Assign, Seq, If, While, ValOf, Expr, Cond, Skip} = require './types' %}
@{% isKeyword = (x) -> x in ['true', 'false', 'if', 'then', 'else', 'while', 'do'] %}
@{% strip = (d) -> d.filter((x) -> x != null) %}
@{% striphead = (d) -> strip(d)[0] %}


# A program
Main -> _ Stmt _ {% striphead %}
      | _ Expr _ {% striphead %}
      | _ Cond _ {% striphead %}

# Statement
Stmt -> "()"              {% -> new Skip %}
      | Var _ "=" _ Expr  {% (d) -> new Assign var: d[0], value: d[4] %}
      | Stmt _ ";" _ Stmt {% (d) -> new Seq s1: d[0], s2: d[4] %}
      | "if" __ Cond __ "then" __ Body __ "else" __ Body
                          {% (d) -> new If cond:d[2], st: d[6], sf:d[10] %}
      | "while" __ Cond __ "do" __ Body
                          {% (d) -> new While cond:d[2], body:d[6] %}
Body -> Stmt              {% id %}
      | "{" _ Stmt _ "}"  {% (d) -> d[2] %} # optional brackets (both)
# TODO: break, continue, exit

# Expression
Expr -> Nr                {% id %}
      | Var               {% (d) -> new ValOf  var:d[0] %}
      | Expr _ IOp _ Expr {% (d) -> new Expr e1: d[0], op:d[2], e2:d[4] %}
      | "(" _ Expr _ ")"  {% (d) -> return d[2] %}

# Conditional
Cond -> Bl                {% id %}
      | Expr _ BOp _ Expr {% (d) -> new Cond e1:d[0], op:d[2], e2:d[4] %}



# Arithmetic Operator
IOp -> "*"  {% id %}
     | "/"  {% id %}
     | "+"  {% id %}
     | "-"  {% id %}
#    | "%"  {% id %}

# Boolean Operators
BOp -> "==" {% id %}
     | "/=" {% id %}
     | "<"  {% id %}
     | ">"  {% id %}
     | "<=" {% id %}
     | ">=" {% id %}


# Boolean literal
Bl -> "true"          {% -> true %}
    | "false"         {% -> false %}

# Number literal
Nr    -> Float        {% id %}
       | Int          {% id %}
Float -> Int "." Int  {% (d) -> parseFloat(d[0] + '.' + d[2]) %}
Int   -> [0-9]:+      {% (d) -> parseInt  (d[0].join(''))      %}



# Variable (alphanumeric & underscores)
Var -> [a-z_] [\w_]:* {% (d) -> d[0] + d[1].join '' %}  # starts with letter/underscore, continues with alphanumeric/_
#{% (d) -> name=d[0].join(''); if(isKeyword(name)) throw new Error(name + " is a keyword, it can't be used as a variable name"); return name %}



# Whitespace
__ -> [\s]:+          {% -> null %}  # required whitespace
_  -> [\s]:*          {% -> null %}  # optional whitespace

