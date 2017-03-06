{Assign, Seq, If, While, ValOf, Expr, Cond, Action} = require './types'

func =
  # iop
  '+': (a, b) -> a + b
  '-': (a, b) -> a - b
  '*': (a, b) -> a * b
  '/': (a, b) -> a / b # TODO: check for division by zero!
  '%': (a, b) -> a % b

  # bop
  '==': (a, b) -> a == b
  '!=': (a, b) -> a != b
  '<':  (a, b) -> a <  b
  '<=': (a, b) -> a <= b
  '>':  (a, b) -> a >  b
  '>=': (a, b) -> a >= b


isa = (x, type) ->
  # TODO: use case (switch)?
  if type is 'iop'
    return x in [
      '+'
      '-'
      '*'
      '/'
      '%'
    ]

  if type is 'bop'
    return x in [
      '=='
      '!='
      '<'
      '<='
      '>'
      '>='
    ]

  return false


clone = (obj) ->
  cloned = {}
  cloned[key] = obj[key] for key of obj
  return cloned


# commands, stack, memory
trans = ({c, s, m}) ->
  # TODO: refactor names (h - head, etc)
  [h, t...] = c

  # TODO: unify typeof x; x.type; isa(x, type); x is ...
  # IntCt, BoolCt
  if typeof h in ['number', 'boolean']
    return { c:t, s:[h, s...], m }

  # Loc
  if h.type is 'valof'
    if h.var not of m
      throw new Error "#{h.var} is undefined"
    n = m[h.var]
    return { c:t, s:[n, s...], m }

  # IntOpC, BoolOpC
  if h.type in ['expr', 'cond']
    return { c:[h.e1, h.e2, h.op, t...], s, m }

  # IntOp, BoolOp
  if isa(h, 'iop') || isa(h, 'bop')
    [n2, n1, st...] = s
    f = func[h]
    n = f(n1, n2)
    return { c:t, s:[n, st...], m }


  # Skip
  if h is '()'
    return {c:t, s, m}

  # AtribC
  if h instanceof Assign
    return { c:[h.value, 'assign', t...], s:[h.var, s...], m }

  # Atrib
  if h is 'assign'
    [n, v, st...] = s
    newMem = clone m # maintain immutability of memory from call to call
    newMem[v] = n # TODO fix for   x := !x - 1
    return { c:t, s:st, m:newMem }

  # CondC
  if h.type is 'if'
    # Use 'branch' as a command symbol in C to differentiate it from the statemtnt 'if'
    return { c:[h.cond, 'branch', t...], s:[h.st, h.sf, s...], m }

  # CondT, CondF
  if h is 'branch'
    [b, stmtT, stmtF, st...] = s
    comm = if b then stmtT else stmtF
    return { c:[comm, t...], s:st, m }

  # Secv
  if h.type is 'seq'
    return { c:[h.s1, h.s2, t...], s, m }


  # IterC
  if h.type is 'while'
    return { c:[h.cond, 'loop', t...], s:[h.cond, h.body, s...], m }

  # IterT, IterF
  if h is 'loop'
    [loopAgain, cond, body, st...] = s
    if loopAgain
      whileStmt = {type: 'while', cond, body}
      return {c:[body, whileStmt, t...], s:st, m}
    else
      return {c:t, s:st, m}

  throw new Error("#{h} didn't match anything for transition")


MAX_N_STATES = 1000

evaluate = (program) ->
  current =  # initial, empty state
    c: [program]
    s: []
    m: {}
  states = [current]

  # Repeatedly apply the transition function, returning the state at each step
  while current.c.length > 0
    current = trans(current)
    states.push current

    if states.length > MAX_N_STATES
      console.warn "maximum number of states reached (#{MAX_N_STATES}), stopping"
      break

  return states


finalState = (program) ->
  states = evaluate program
  states[states.length - 1]



module.exports = {trans, evaluate, finalState}
