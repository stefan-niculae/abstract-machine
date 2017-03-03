#@preprocessor coffee

# editor: https://omrelli.ug/nearley-playground/

@{% function head(d) { return d[0] } %}
@{% function dhead(d) { return d[0][0] } %}
@{% function strip(d) { return d.filter(function(x) {return x!==null})} %}
@{% function type(name) { return function(d) { return {type:name, val:d[0]} } } %}



# A program
Main ->  Stmt {% head %}
	  | Expr {% head %}
	  | Cond {% head %}

# Expression
Expr -> Nr {% dhead %} # ?? why double head?
	  | "!" _ Var {% function arg2(d) { return {type:"valof", var:d[2]} } %}
	  | Expr _ IOp _ Expr {% strip %}

# Conditional
Cond -> Bl {% dhead %} # ?? why double head?
	  | Expr _ BOp _ Expr {% function(d) {return {type:'cond', val: strip(d)}} %}

# Statement
Stmt -> "()" 				# skip
	  | Var _ ":=" _ Expr {% strip %} 	# assign
	  | Stmt _ ";" _ Stmt {% strip %}		# sequence
	  | "if" _ Cond _ "then" _ Stmt _ "else" _ Stmt {% strip %}
	  | "while" _ Cond _ "do" _ Stmt {% strip %}



# Arithmetic Operator
IOp -> "+" {% type('iop') %}
	 | "-" {% type('iop') %}
	 | "/" {% type('iop') %}
	 | "*" {% type('iop') %}
	 | "%" {% type('iop') %}

# TODO: unary operators "^", "Not"?

# Boolean Operators
BOp -> "==" {% type('bop') %}
	 | "/=" {% type('bop') %}
	 | "<" {% type('bop') %}
	 | ">" {% type('bop') %}
	 | "<=" {% type('bop') %}
	 | ">=" {% type('bop') %}

# TODO: LOp "And", "Or", "Xor"?


# TODO: break, continue, exit?



# Variable
Var -> [\w_]:+ {% dhead %} # words, letters or underscores

# Boolean literal
Bl -> "true" | "false"

# Number literal
Nr -> Float | Int
Float -> Int "." Int   {% function(d) {return parseFloat(d[0] + '.' + d[2])  } %}
# TODO negative number literals!
Int -> [0-9]:+        {% function(d) {return parseInt(d[0].join(""))   } %}



# TODO: newline
# Whitespace
_ -> [\s]:*     {% function(d) {return null } %}
