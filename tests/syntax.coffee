parse = require '../src/parser'
{Assign, Seq, If, While, ValOf, Expr, Cond, Skip, Break, Continue, Exit} = require '../src/types'

# TODO tests for break & continue


describe 'The parser for literals', ->

  it 'can parse an int', ->
    input = '7'
    expect(parse(input)).toEqual 7

  it 'can parse a float', ->
    input = '7.5'
    expect(parse(input)).toEqual 7.5

  it 'can parse `true`', ->
    input = 'true'
    expect(parse(input)).toEqual true

  it 'can parse `false`', ->
    input = 'false'
    expect(parse(input)).toEqual false



describe 'The parser for conditions & expressions', ->

  it 'can parse a boolean condition', ->
    input = '0 < 1'
    expect(parse(input)).toEqual new Cond
      e1: 0
      op: '<'
      e2: 1

  it 'can parse an arithmetic expression', ->
    input = '1 + 2'
    expect(parse(input)).toEqual new Expr
      e1: 1
      op: '+'
      e2: 2

  it 'can parse dereferencing', ->
    input = 'var'
    expect(parse(input)).toEqual new ValOf
      var: 'var'

  it 'does correct order of operations (a + b/c)', ->
    input = '4 + 6/2'
    expect(parse(input)).toEqual new Expr
      e1: 4
      op: '+'
      e2: new Expr
        e1: 6
        op: '/'
        e2: 2

  # TODO
#  it 'does correct order of operations (a/b + c)', ->
#    input = '4/2 + 6'
#    expect(parse(input)).toEqual new Expr
#      e1: new Expr
#        e1: 4
#        op: '/'
#        e2: 2
#      op: '+'
#      e2: 6

  it 'does arith expr parens (div)', ->
    input = '4 + (6/2)'
    expect(parse(input)).toEqual new Expr
      e1: 4
      op: '+'
      e2: new Expr
        e1: 6
        op: '/'
        e2: 2


  it 'does arith expr parens (plus)', ->
    input = '(4+6) / 2'
    expect(parse(input)).toEqual new Expr
      e1: new Expr
        e1: 4
        op: '+'
        e2: 6
      op: '/'
      e2: 2

  # TODO: currently they bind to the left
#  it 'sequences operations to the right', ->
#    input = '10 / 2 * 4' # ~> (10/2) * 4
#    expect(parse(input)).toEqual new Expr
#      e1: new Expr
#        e1: 10
#        op: '/'
#        e2: 2
#      op: '*'
#      e2: 4




describe 'The parser for commands', ->

  it 'can parse `skip`', ->
    input = '()'
    expect(parse(input)).toEqual new Skip

  it 'can parse literal assignment', ->
    input = 'var = 0'
    expect(parse(input)).toEqual new Assign
      var: 'var'
      value: 0
      msg: 'from tests'

  it 'can parse expression assignment', ->
    input = 'x = y + 1'
    expect(parse(input)).toEqual new Assign
      var: 'x'
      value: new Expr
        e1: new ValOf var: 'y'
        op: '+'
        e2: 1

  # TODO
#  it "doesn't allow keyword assignment", ->
#    input = 'false = 5'
#    expect(-> parse(input)).toThrowError "false is a keyword, it can't be used as a variable name"

  # TODO
#  it "doesn't allow keyword comparison", ->
#    input = 'while < 5'
#    expect(-> parse(input)).toThrowError "while is a keyword, it can't be used as a variable name"

  it 'does not allow variables starting with a digit', ->
    input = '1var = 0'
    expect(-> parse(input)).toThrowError

  it 'can parse sequenced statements', ->
    input = 'x = 0; ()'
    expect(parse(input)).toEqual new Seq
      s1: new Assign
        var: 'x'
        value: 0
      s2: new Skip

  it 'can parse sequencing with newlines', ->
    input = """
      x = 0;

      y = 1
    """
    expect(parse(input)).toEqual new Seq
      s1: new Assign
        var: "x"
        value: 0
      s2: new Assign
        var: "y"
        value: 1

  it 'can parse the exit command', ->
    input = 'exit!'
    expect(parse(input)).toEqual new Exit





describe 'The parser for control structures', ->

  it 'can parse an if statement', ->
    input = 'if true then () else x = 0'
    expect(parse(input)).toEqual new If
      cond: true
      st: new Skip
      sf: new Assign
        var: 'x'
        value: 0

  it 'can parse a while statement', ->
    input = 'while false do ()'
    expect(parse(input)).toEqual new While
      cond: false
      body: new Skip

  it 'can parse nested control statements', ->
    input = """
    while false do
      if true then
        ()
      else
        ()
    """
    expect(parse(input)).toEqual new While
      cond: false
      body: new If
        cond: true
        st: new Skip
        sf: new Skip

  it 'accepts braces for if statements', ->
    input = """
    if true then {
      ()
    }
    else {
      ()
    }
    """
    expect(parse(input)).toEqual new If
      cond: true
      st: new Skip
      sf: new Skip

  it 'accepts braces for while statements', ->
    input = """
    while true do {
      ();
      ()
    }
    """
    expect(parse(input)).toEqual new While
      cond: true
      body: new Seq
        s1: new Skip
        s2: new Skip

  it 'binds only the first statement after to the while (when no braces)', ->
    input = """
    while true do
        ();
    ()
    """
    expect(parse(input)).toEqual new Seq
      s1: new While
        cond: true
        body: new Skip
      s2: new Skip


describe 'The parser for more complex programs', ->

  it 'can parse multiple-statements while', ->
    input = """
    while 0 <= x do {
      sum = sum + x;
      x = x - 1
    }
    """
    expect(parse(input)).toEqual new While
      cond: new Cond
        e1: 0
        op: '<='
        e2: new ValOf var: 'x'

      body: new Seq
        s1: new Assign
          var: 'sum'
          value: new Expr
            e1: new ValOf var: 'sum'
            op: '+'
            e2: new ValOf var: 'x'

        s2: new Assign
          var: 'x'
          value: new Expr
            e1: new ValOf var: 'x'
            op: '-'
            e2: 1

  it 'can parse min', ->
    input = """
    a = 1;
    b = 7;
    if a < b then
      min = a
    else
      min = b
    """
    expect(parse(input)).toEqual new Seq
      s1: new Seq
        s1: new Assign
          var: 'a'
          value: 1
        s2: new Assign
          var: 'b'
          value: 7
      s2: new If
        cond: new Cond
          e1: new ValOf var: 'a'
          op: '<'
          e2: new ValOf var: 'b'
        st: new Assign
          var: 'min'
          value: new ValOf var: 'a'
        sf: new Assign
          var: 'min'
          value: new ValOf var: 'b'
