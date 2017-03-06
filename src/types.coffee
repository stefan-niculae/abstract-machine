"""
Flat types:
 - numbers               int float
 - booleans              true false
 - arithmetic operators  + - * /
 - boolean operators     == != < > <= >=
 - skip statement        ()
"""


assertType = (obj, types...) ->
  for type in types
    if typeof type is 'string'
      # Check for value when type is a string, eg: '+', '()'
      if obj is type
        return
    else
      # Check for type when type is a class, eg: Seq, Cond
      if obj instanceof type
        return

      if type is String # special case for variables, because ('abc' instanceof String) is false
        return if typeof obj is 'string'
      if type is Number # special case for int & float literals, because (5 instanceof Number) is false
        return if typeof obj is 'number'
      if type is Boolean # special case for boolean literals, because (true instanceof Number) is false
        return if typeof obj is 'boolean'


  throw new Error "expected #{obj} to be one of: #{types.join(', ')}"

class Assign
  constructor: ({@var, @value}) ->
    assertType @var, String
    assertType @value, Expr, Number, ValOf

class Seq
  constructor: ({@s1, @s2}) ->
    assertType @s1, If, While, Seq, Assign, Skip, Break, Continue, Exit
    assertType @s2, If, While, Seq, Assign, Skip, Break, Continue, Exit

class If
  constructor: ({@cond, @st, @sf}) ->
    assertType @cond, Cond, Boolean
    assertType @st, If, While, Seq, Assign, Skip, Break, Continue, Exit
    assertType @sf, If, While, Seq, Assign, Skip, Break, Continue, Exit

class While
  constructor: ({@cond, @body}) ->
    assertType @cond, Cond, Boolean
    assertType @body, If, While, Seq, Assign, Skip

class ValOf
  # Intentionally not flat for better readability
  constructor: ({@var}) ->
    assertType @var, String

class Expr
  constructor: ({@e1, @op, @e2}) ->
    assertType @e1, Expr, Number, ValOf
    assertType @op, '+', '-', '*', '/'
    assertType @e2, Expr, Number, ValOf

class Cond
  constructor: ({@e1, @op, @e2}) ->
    assertType @e1, Expr, Number, ValOf
    assertType @op, '==', '!=', '<', '>', '<=', '>='
    assertType @e2, Expr, Number, ValOf


# Intentionally not flat for better readability
class Save

class Branch

class Loop

class Skip

class Break

class Continue

class Exit


module.exports = {
  Assign, Seq, If, While, ValOf, Expr, Cond,
  Save, Branch, Loop, Skip,
  Break, Continue, Exit
}