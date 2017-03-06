{trans, finalState} = require '../src/evaluator'
{Assign, Seq, If, While, ValOf, Expr, Cond, Save, Branch, Loop, Skip} = require '../src/types'



describe 'The transition function for arithmetic expressions', ->

  it 'can move an int from c to s', ->
    state =
      c: [1, new Skip]
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [1, 8]
      m: {}

  it 'can put the value of a var from m in s', ->
    state =
      c: [new ValOf(var: 'x'), new Skip]
      s: [8]
      m: x: 1, y: 2
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [1, 8]
      m: x: 1, y: 2

  it 'throws an exception on undefined var', ->
    state =
      c: [new ValOf(var: 'x')]
      s: []
      m: {}
    expect(-> trans(state)).toThrowError 'x is undefined'

  it 'can transform an expression into post-fix form', ->
    state =
      c: [new Expr(e1: 1, op: '+', e2: 2), new Skip]
      s: []
      m: {}
    expect(trans(state)).toEqual
      c: [1, 2, '+', new Skip]
      s: []
      m: {}

  it 'can do addition', ->
    state =
      c: ['+', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [3]
      m: {}

  it 'can do subtraction', ->
    state =
      c: ['-', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [-1]
      m: {}

  it 'can do multiplication', ->
    state =
      c: ['*', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [2]
      m: {}

  it 'can do division', ->
    state =
      c: ['/', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [0.5]
      m: {}

  it 'cannot divide by zero', ->
    state =
      c: ['/']
      s: [0, 1]
      m: {}
    expect(-> trans(state)).toThrowError 'division by zero'

  it 'can do modulus', ->
    state =
      c: ['%', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [1]
      m: {}



describe 'The transition function for bool conditions', ->

  it 'can move a bool from c to s', ->
    state =
      c: [true, new Skip]
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [true, 8]
      m: {}

  it 'can transform a condition into post-fix form', ->
    state =
      c: [new Cond(e1: 1, op: '<', e2: 2), new Skip]
      s: []
      m: {}
    expect(trans(state)).toEqual
      c: [1, 2, '<', new Skip]
      s: []
      m: {}

  it 'can do eq comparison', ->
    state =
      c: ['==', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [false]
      m: {}

  it 'can do neq comparison', ->
    state =
      c: ['!=', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [true]
      m: {}

  it 'can do lt comparison', ->
    state =
      c: ['<', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [true]
      m: {}

  it 'can do lte comparison', ->
    state =
      c: ['<=', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [true]
      m: {}

  it 'can do gt comparison', ->
    state =
      c: ['>', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [false]
      m: {}

  it 'can do gte comparison', ->
    state =
      c: ['>=', new Skip]
      s: [2, 1]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [false]
      m: {}



describe 'The transition function for commands', ->

  it 'can skip', ->
    state =
      c: [new Skip, new Skip]
      s: []
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: []
      m: {}

  it 'can disperse an assignment statement to c and s', ->
    state =
      c: [new Assign(var: 'x', value: 1), new Skip]
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: [1, new Save, new Skip]
      s: ['x', 8]
      m: {}

  it 'can assign in memory', ->
    state =
      c: [new Save, new Skip]
      s: [1, 'x', 8]
      m: y: 2
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [8]
      m: x: 1, y: 2


  it 'can sequence statements', ->
    assignment = new Assign var: 'x', value: 1
    state =
      c: [new Seq(s1:new Skip, s2:assignment), new Skip]
      s: []
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip, assignment, new Skip]
      s: []
      m: {}



describe 'The transition function for branching and looping', ->

  it 'can disperse an if statements to c and s', ->
    assignment = new Assign var: 'x', value: 1
    state =
      c: [new If(cond: true, st: new Skip, sf: assignment), new Skip]
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: [true, new Branch, new Skip]
      s: [new Skip, assignment, 8]
      m: {}


  it 'can branch based on the top of the stack (true)', ->
    assignment = new Assign var: 'x', value: 1
    state =
      c: [new Branch, new Skip]
      s: [true, new Skip, assignment, 8]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip, new Skip]
      s: [8]
      m: {}

  it 'can branch based on the top of the stack (false)', ->
    assignment = new Assign var: 'x', value: 1
    state =
      c: [new Branch, new Skip]
      s: [false, new Skip, assignment, 8]
      m: {}
    expect(trans(state)).toEqual
      c: [assignment, new Skip]
      s: [8]
      m: {}

  it 'can disperse a while statement to c and s ', ->
    state =
      c: [new While(cond:true, body:new Skip), new Skip]
      s: [8]
      m: {}
    expect(trans(state)).toEqual
      c: [true, new Loop, new Skip]
      s: [true, new Skip, 8]
      m: {}

  it 'can loop when the top of the stack is false', ->
    cond = new Cond e1: 1, op: '<', e2: 2
    body = new Assign var: 'x', value: 0
    state =
      c: [new Loop, new Skip]
      s: [false, cond, body, 8]
      m: {}
    expect(trans(state)).toEqual
      c: [new Skip]
      s: [8]
      m: {}

  it 'can loop when the top of the stack is true', ->
    cond = new Cond e1: 1, op: '<', e2: 2
    body = new Assign var: 'x', value: 0
    state =
      c: [new Loop, new Skip]
      s: [true, cond, body, 8]
      m: {}
    expect(trans(state)).toEqual
      c: [body, new While({cond, body}), new Skip]
      s: [8]
      m: {}



describe 'The evaluation function', ->

  it 'can do sequencing', ->
    result = finalState new Seq
      s1: new Assign var: 'x', value: 1
      s2: new Assign var: 'x', value: 2

    expect(result.c).toEqual []
    expect(result.m).toEqual {x: 2}

  it 'guards against infinite cycles', ->
    state =
      c: [new While cond: true, body: new Skip]
      s: []
      m: {}
    expect(trans(state).m).toEqual {}

  # TODO self assignment in while
  """
  x = 3;
  while x > 0 do
    x = x - 1
  """