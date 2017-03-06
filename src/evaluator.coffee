{Assign, Seq, If, While, ValOf, Expr, Cond, Save, Branch, Loop, Skip, Break, Continue, Exit} = require './types'

func =
  # iop
  '+': (a, b) -> a + b
  '-': (a, b) -> a - b
  '*': (a, b) -> a * b
  '/': (a, b) -> if b isnt 0 then a / b else throw new Error "division by zero"
  '%': (a, b) -> a % b

  # bop
  '==': (a, b) -> a == b
  '!=': (a, b) -> a != b
  '<':  (a, b) -> a <  b
  '<=': (a, b) -> a <= b
  '>':  (a, b) -> a >  b
  '>=': (a, b) -> a >= b


isa = (x, type) ->
  switch type
    when 'iop' then x in [
      '+'
      '-'
      '*'
      '/'
      '%'
      ]

    when 'bop' then x in [
      '=='
      '!='
      '<'
      '<='
      '>'
      '>='
    ]
    else false


clone = (obj) ->
  cloned = {}
  cloned[key] = obj[key] for key of obj
  return cloned


# commands, stack, memory
trans = ({c, s, m}) ->
  [head, tail...] = c

  # IntCt, BoolCt
  if typeof head in ['number', 'boolean']
    return {
      c: tail
      s: [head, s...]
      m
    }

  # Loc
  if head instanceof ValOf
    if head.var not of m
      throw new Error "#{head.var} is undefined"
    n = m[head.var]
    return {
      c: tail
      s: [n, s...]
      m
    }

  # IntOpC, BoolOpC
  if head instanceof Expr or head instanceof Cond
    return {
      c: [head.e1, head.e2, head.op, tail...],
      s,
      m
    }

  # IntOp, BoolOp
  if isa(head, 'iop') || isa(head, 'bop')
    [n2, n1, sTail...] = s
    f = func[head]
    n = f(n1, n2)
    return {
      c: tail
      s: [n, sTail...]
      m
    }


  # Skip
  if head instanceof Skip
    return {
      c: tail
      s
      m
    }

  # AtribC
  if head instanceof Assign
    return {
      c: [head.value, new Save, tail...]
      s: [head.var, s...]
      m
    }

  # Atrib
  if head instanceof Save
    [n, v, sTail...] = s
    newMem = clone m # maintain immutability of memory from call to call
    newMem[v] = n
    return {
      c: tail
      s: sTail
      m: newMem
    }

  # CondC
  if head instanceof If
    return {
      c: [head.cond, new Branch, tail...]
      s: [head.st, head.sf, s...]
      m
    }

  # CondT, CondF
  if head instanceof Branch
    [b, stmtT, stmtF, sTail...] = s
    comm = if b then stmtT else stmtF
    return {
      c: [comm, tail...]
      s: sTail
      m
    }

  # Secv
  if head instanceof Seq
    return {
      c: [head.s1, head.s2, tail...]
      s
      m
    }


  # IterC
  if head instanceof While
    return {
      c:[head.cond, new Loop, tail...]
      s:[head.cond, head.body, s...]
      m
    }

  # IterT, IterF
  if head instanceof Loop
    [loopAgain, cond, body, sTail...] = s
    if loopAgain
      whileStmt = new While {cond, body}
      return {
        c: [body, whileStmt, tail...]
        s: sTail
        m
      }
    else
      return {
        c: tail
        s: sTail
        m
      }

  if head instanceof Exit
    return {
      c: tail
      s
      m
    }

  throw new Error("#{head} didn't match anything for transition")


MAX_N_STATES = 1000

evaluate = (program) ->
  current =  # initial, empty state
    c: [program]
    s: []
    m: {}
  states = [current]

  willExit = false
  # Repeatedly apply the transition function, returning the state at each step
  while current.c.length > 0
    willExit = true if current.c[0] instanceof Exit

    current = trans(current)
    states.push current

    if willExit
      console.warn 'exiting'
      break

    if states.length > MAX_N_STATES
      console.warn "maximum number of states reached (#{MAX_N_STATES}), stopping"
      break

  return states


finalState = (program) ->
  states = evaluate program
  states[states.length - 1]



module.exports = {trans, evaluate, finalState}
