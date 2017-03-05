@preprocessor coffee
@{% %} # fix for coffee preprocessor...


@{% isKeyword = (x) -> x in ['true', 'false', 'if', 'then', 'else', 'while', 'do'] %}
@{% strip = (d) -> d.filter((x) -> x != null) %}
@{% striphead = (d) -> strip(d)[0] %}
# TODO function composition?


# A program
Main -> _ Stmt _ {% striphead %}
	  | _ Expr _ {% striphead %} # DEBUG, to remove
	  | _ Cond _ {% striphead %} # DEBUG, to remove

# Statement
Stmt -> "()" 			  {% id %} # skip
	  | Var _ ":=" _ Expr {% (d) -> type:'assign', var: d[0], val: d[4] %} 	# assign
	  | Stmt _ ";" _ Stmt {% (d) -> type:'seq',    s1:  d[0], s2:  d[4] %}	# sequence
	  | "if" __ Cond __ "then" __ Body __ "else" __ Body
	  					  {% (d) -> type:'if',     cond:d[2], st:  d[6], sf:d[10] %}
	  | "while" __ Cond __ "do" __ Body
	  					  {% (d) -> type:'while',  cond:d[2], body:d[6] %}
Body -> Stmt              {% id %}
	  | "{" _ Stmt _ "}"  {% (d) -> d[2] %} # optional brackets (both)
# TODO: break, continue, exit

# Expression
Expr -> Nr 				  {% id %}
	  | "!" _ Var 		  {% (d) -> type:'valof',  var:d[2] %}
	  | Expr _ IOp _ Expr {% (d) -> type:'expr',   e1: d[0],  op:d[2], e2:d[4] %}
	  | "(" _ Expr _ ")"  {% (d) -> return d[2] %}

# Conditional
Cond -> Bl                {% id %}
	  | Expr _ BOp _ Expr {% (d) -> type:'cond',   e1:d[0],   op:d[2], e2:d[4] %}



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

# TODO: LOp "And", "Or", "Xor"?
# TODO: unary operators "^", "Not"?



# Boolean literal
Bl -> "true"         {% -> true %}
    | "false"        {% -> false %}

# Number literal
Nr    -> Float       {% id %}
       | Int         {% id %}
Float -> Int "." Int {% (d) -> parseFloat(d[0] + '.' + d[2]) %}
Int   -> [0-9]:+     {% (d) -> parseInt  (d[0].join(''))      %}
# TODO: negative number literals?



# Variable (alphanumeric & underscores)
Var -> [\w_]:+      {% (d) -> d[0].join '' %}
#{% (d) -> name=d[0].join(''); if(isKeyword(name)) throw new Error(name + " is a keyword, it can't be used as a variable name"); return name %}
# TODO check if not in keywords!



# Whitespace
__ -> [\s\\n]:+      {% -> null %}  # required whitespace
_  -> [\s\\n]:*      {% -> null %}  # optional whitespace
