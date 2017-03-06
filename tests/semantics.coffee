{trans, finalState} = require '../src/evaluator'



describe 'The transition function for int expressions', ->

  it 'can move an int from c to s', ->
    state =
      c: [1, '()']
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [1, 8]
      m: {}

  it 'can put the value of a var from m in s', ->
    state =
      c: [{type: 'valof', var: 'x'}, '()']
      s: [8]
      m: x: 1, y: 2
    expect(trans(state)).toEqual
      c: ['()']
      s: [1, 8]
      m: x: 1, y: 2

  it 'throws an exception on undefined var', ->
    state =
      c: [{type: 'valof', var: 'x'}]
      s: []
      m: {}
    expect(-> trans(state)).toThrowError 'x is undefined'

  it 'can transform an expression into post-fix form', ->
    state =
      c: [{type: 'expr', e1: 1, op: '+', e2: 2}, '()']
      s: []
      m: {}
    expect(trans(state)).toEqual
      c: [1, 2, '+', '()']
      s: []
      m: {}

  it 'can do addition', ->
    state =
      c: ['+', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [3]
      m: {}

  it 'can do subtraction', ->
    state =
      c: ['-', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [-1]
      m: {}

  it 'can do multiplication', ->
    state =
      c: ['*', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [2]
      m: {}

  it 'can do division', ->
    state =
      c: ['/', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [0.5]
      m: {}

  it 'can do modulus', ->
    state =
      c: ['%', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [1]
      m: {}



describe 'The transition function for bool conditions', ->

  it 'can move a bool from c to s', ->
    state =
      c: [true, '()']
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [true, 8]
      m: {}

  it 'can transform a condition into post-fix form', ->
    state =
      c: [{type: 'cond', e1: 1, op: '<', e2: 2}, '()']
      s: []
      m: {}
    expect(trans(state)).toEqual
      c: [1, 2, '<', '()']
      s: []
      m: {}

  it 'can do eq comparison', ->
    state =
      c: ['==', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [false]
      m: {}

  it 'can do neq comparison', ->
    state =
      c: ['!=', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [true]
      m: {}

  it 'can do lt comparison', ->
    state =
      c: ['<', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [true]
      m: {}

  it 'can do lte comparison', ->
    state =
      c: ['<=', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [true]
      m: {}

  it 'can do gt comparison', ->
    state =
      c: ['>', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [false]
      m: {}

  it 'can do gte comparison', ->
    state =
      c: ['>=', '()']
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [false]
      m: {}



describe 'The transition function for commands', ->

  it 'can skip', ->
    state =
      c: ['()', '()']
      s: []
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: []
      m: {}

  it 'can disperse an assignment statement to c and s', ->
    state =
      c: [{type: 'assign', var: 'x', val: 1}, '()']
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: [1, 'assign', '()']
      s: ['x', 8]
      m: {}

  it 'can assign in memory', ->
    state =
      c: ['assign', '()']
      s: [1, 'x', 8]
      m: y: 2
    expect(trans(state)).toEqual
      c: ['()']
      s: [8]
      m: x: 1, y: 2


  it 'can sequence statements', ->
    assignment = {type: 'assign', var: 'x', val: 1}
    state =
      c: [{type:'seq', s1:'()', s2:assignment}, '()']
      s: []
      m: {}
    expect(trans(state)).toEqual
      c: ['()', assignment, '()']
      s: []
      m: {}



describe 'The transition function for branching and looping', ->

  it 'can disperse an if statements to c and s', ->
    assignment = {type: 'assign', var: 'x', val: 1}
    state =
      c: [{type: 'if', cond: true, st: '()', sf: assignment }, '()']
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: [true, 'branch', '()']
      s: ['()', assignment, 8]
      m: {}


  it 'can branch based on the top of the stack (true)', ->
    assignment = {type: 'assign', var: 'x', val: 1}
    state =
      c: ['branch', '()']
      s: [true, '()', assignment, 8]
      m: {}
    expect(trans(state)).toEqual
      c: ['()', '()']
      s: [8]
      m: {}

  it 'can branch based on the top of the stack (false)', ->
    assignment = {type: 'assign', var: 'x', val: 1}
    state =
      c: ['branch', '()']
      s: [false, '()', assignment, 8]
      m: {}
    expect(trans(state)).toEqual
      c: [assignment, '()']
      s: [8]
      m: {}

  it 'can disperse a while statement to c and s ', ->
    state =
      c: [{type:'while', cond:true, body:'()'}, '()']
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: [true, 'loop', '()']
      s: [true, '()', 8]
      m: {}

  it 'can loop when the top of the stack is false', ->
    cond = {type: 'cond', e1: 1, op: '<', e2: 2}
    body = {type: 'assign', var: 'x', val: 0}
    state =
      c: ['loop', '()']
      s: [false, cond, body, 8]
      m: {}
    expect(trans(state)).toEqual
      c: ['()']
      s: [8]
      m: {}

  it 'can loop when the top of the stack is true', ->
    cond = {type: 'cond', e1: 1, op: '<', e2: 2}
    body = {type: 'assign', var: 'x', val: 0}
    state =
      c: ['loop', '()']
      s: [true, cond, body, 8]
      m: {}
    expect(trans(state)).toEqual
      c: [body, {type:'while', cond, body}, '()']
      s: [8]
      m: {}



describe 'The evaluation function', ->

  it 'can do sequencing', ->
    result = finalState
      type: 'seq'
      s1: {type: 'assign', var: 'x', val: 1}
      s2: {type: 'assign', var: 'x', val: 2}

    expect(result.c).toEqual []
    expect(result.m).toEqual {x: 2}


  # TODO self assignment in while
  """
  x = 3;
  while x > 0 do
    x = x - 1
  """

  # TODO infinite cycle guard
  """
  while true do ()
  """
