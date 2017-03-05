#@preprocessor coffee

@{% function isKeyword(x) { return ['true', 'false', 'if', 'then', 'else', 'while', 'do'].indexOf(x) != -1} %}
@{% function strip(d) { return d.filter(function(x) {return x !== null}) }%}
@{% function striphead(d) {return strip(d)[0]} %}
# TODO function composition?


# A program
Main -> _ Stmt _ {% striphead %}
	  | _ Expr _ {% striphead %} # DEBUG, to remove
	  | _ Cond _ {% striphead %} # DEBUG, to remove

# Statement
Stmt -> "()" 			  {% id %} # skip
	  | Var _ ":=" _ Expr {% function(d) { return {type:'assign', var: d[0], val: d[4]} } %} 	# assign
	  | Stmt _ ";" _ Stmt {% function(d) { return {type:'seq',    s1:  d[0], s2:  d[4]} } %}	# sequence
	  | "if" __ Cond __ "then" __ Body __ "else" __ Body
	  					  {% function(d) { return {type:'if',     cond:d[2], st:  d[6], sf:d[10]} } %}
	  | "while" __ Cond __ "do" __ Body
	  					  {% function(d) { return {type:'while',  cond:d[2], body:d[6]} } %}
Body -> Stmt              {% id %}
	  | "{" _ Stmt _ "}"  {% function(d) { return d[2] } %} # optional brackets (both)
# TODO: break, continue, exit

# Expression
Expr -> Nr 				  {% id %}
	  | "!" _ Var 		  {% function(d) { return {type:'valof',  var:d[2]} } %}
	  | Expr _ IOp _ Expr {% function(d) { return {type:'expr',   e1: d[0],  op:d[2], e2:d[4]} } %}
	  | "(" _ Expr _ ")"  {% function(d) { return d[2] } %}

# Conditional
Cond -> Bl {% id %}
	  | Expr _ BOp _ Expr {% function(d) { return {type:'cond',   e1:d[0],   op:d[2], e2:d[4]} } %}



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
Bl -> "true"         {% function(d) { return true } %}
    | "false"        {% function(d) { return false } %}

# Number literal
Nr    -> Float       {% id %}
       | Int         {% id %}
Float -> Int "." Int {% function(d) { return parseFloat(d[0] + '.' + d[2]) } %}
Int   -> [0-9]:+     {% function(d) { return parseInt  (d[0].join(''))     } %}
# TODO: negative number literals?



# Variable (alphanumeric & underscores)
Var -> [\w_]:+      {% function(d) {return d[0].join('')
}%} #{% function(d) { var name=d[0].join(''); if(isKeyword(name)) throw new Error(name + " is a keyword, it can't be used as a variable name"); return name  } %}
# TODO check if not in keywords!



# Whitespace
__ -> [\s\\n]:+      {% function(d) { return null } %}  # required whitespace
_  -> [\s\\n]:*      {% function(d) { return null } %}

