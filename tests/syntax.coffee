nearley = require '../nearley' # TODO: import it from npm instead of the local one
grammar = require '../grammar'

# Generate a fresh parser
parseResults = (input) ->
  parser = new nearley.Parser(grammar.ParserRules, grammar.ParserStart)
  parser.feed input
  return parser.results[0]


# TODO: debug grammar (where Main -> Expr, Cond)?
describe 'The parser for literals', ->

  it 'can parse an int', ->
    input = '7'
    expect(parseResults(input)).toEqual 7

  it 'can parse a float', ->
    input = '7.5'
    expect(parseResults(input)).toEqual 7.5

  it 'can parse `true`', ->
    input = 'true'
    expect(parseResults(input)).toEqual true

  it 'can parse `false`', ->
    input = 'false'
    expect(parseResults(input)).toEqual false




describe 'The parser for conditions & expressions', ->

  it 'can parse a boolean condition', ->
    input = '0 < 1'
    expect(parseResults(input)).toEqual
      type: 'cond'
      e1: 0
      op: '<'
      e2: 1

  it 'can parse an arithmetic expression', ->
    input = '1 + 2'
    expect(parseResults(input)).toEqual
      type: 'expr'
      e1: 1
      op: '+'
      e2: 2

  it 'can parse dereferencing', ->
    input = '!var'
    expect(parseResults(input)).toEqual
      type: 'valof'
      var: 'var'

  it 'does correct order of operations (a + b/c)', ->
    input = '4 + 6/2'
    expect(parseResults(input)).toEqual
      type: 'expr'
      e1: 4
      op: '+'
      e2:
        type: 'expr'
        e1: 6
        op: '/'
        e2: 2

  # TODO
#  it 'does correct order of operations (a/b + c)', ->
#    input = '4/2 + 6'
#    expect(parseResults(input)).toEqual
#      type: 'expr'
#      e1:
#        type: 'expr'
#        e1: 4
#        op: '/'
#        e2: 2
#      op: '+'
#      e2: 6

  it 'does arith expr parens (div)', ->
    input = '4 + (6/2)'
    expect(parseResults(input)).toEqual
      type: 'expr'
      e1: 4
      op: '+'
      e2:
        type: 'expr'
        e1: 6
        op: '/'
        e2: 2


  it 'does arith expr parens (plus)', ->
    input = '(4+6) / 2'
    expect(parseResults(input)).toEqual
      type: 'expr'
      e1:
        type: 'expr'
        e1: 4
        op: '+'
        e2: 6
      op: '/'
      e2: 2

  # TODO: currently they bind to the left
#  it 'sequences operations to the right', ->
#    input = '10 / 2 * 4' # ~> (10/2) * 4
#    expect(parseResults(input)).toEqual
#      type: 'expr'
#      e1:
#        type: 'expr'
#        e1: 10
#        op: '/'
#        e2: 2
#      op: '*'
#      e2: 4




describe 'The parser for commands', ->

  it 'can parse `skip`', ->
    input = '()'
    expect(parseResults(input)).toEqual '()'

  it 'can parse literal assignment', ->
    input = 'var := 0'
    expect(parseResults(input)).toEqual
      type: 'assign'
      var: 'var'
      val: 0

  it 'can parse expression assignment', ->
    input = 'x := !y + 1'
    expect(parseResults(input)).toEqual
      type: 'assign'
      var: 'x'
      val:
        type: 'expr'
        e1:
          type: 'valof'
          var: 'y'
        op: '+'
        e2: 1

  # TODO
#  it "doesn't allow keyword assignment", ->
#    input = 'false := 5'
#    expect(-> parseResults(input)).toThrowError "false is a keyword, it can't be used as a variable name"

  # TODO
#  it "doesn't allow keyword comparison", ->
#    input = '!while < 5'
#    expect(-> parseResults(input)).toThrowError "while is a keyword, it can't be used as a variable name"

  it 'can parse sequenced statements', ->
    input = 'x := 0; ()'
    expect(parseResults(input)).toEqual
      type: 'seq'
      s1:
        type: 'assign'
        var: 'x'
        val: 0
      s2: '()'

  it 'can parse sequencing with newlines', ->
    input = """
      x := 0;

      y := 1
    """
    expect(parseResults(input)).toEqual
      type: "seq"
      s1:
        type: "assign"
        var: "x"
        val: 0
      s2:
        type: "assign"
        var: "y"
        val: 1





describe 'The parser for control structures', ->

  it 'can parse an if statement', ->
    input = 'if true then () else x := 0'
    expect(parseResults(input)).toEqual
      type: 'if'
      cond: true
      st: '()'
      sf:
        type: 'assign'
        var: 'x'
        val: 0

  it 'can parse a while statement', ->
    input = 'while false do ()'
    expect(parseResults(input)).toEqual
      type: 'while'
      cond: false
      body: '()'

  it 'can parse nested control statements', ->
    input = """
    while false do
      if true then
        ()
      else
        ()
    """
    expect(parseResults(input)).toEqual
      type: 'while'
      cond: false
      body:
        type: 'if'
        cond: true
        st: '()'
        sf: '()'

  it 'accepts braces for if statements', ->
    input = """
    if true then {
      ()
    }
    else {
      ()
    }
    """
    expect(parseResults(input)).toEqual
      type: 'if'
      cond: true
      st: '()'
      sf: '()'

  it 'accepts braces for while statements', ->
    input = """
    while true do {
      ();
      ()
    }
    """
    expect(parseResults(input)).toEqual
      type: 'while'
      cond: true
      body:
        type: 'seq'
        s1: '()'
        s2: '()'

  it 'binds only the first statement after to the while (when no braces)', ->
    input = """
    while true do
        ();
    ()
    """
    expect(parseResults(input)).toEqual
      type: 'seq'
      s1:
        type: 'while'
        cond: true
        body: '()'
      s2: '()'


describe 'The parser for more complex programs', ->

  it 'can do summation', ->
    input = """
    while 0 <= !x do {
      sum := !sum + !x;
      x := !x - 1
    }
    """
    expect(parseResults(input)).toEqual
      type: 'while'
      cond: type: 'cond', e1: 0, op: '<=', e2: {type: 'valof', var: 'x'}
      body:
        type: 'seq'
        s1:
          type: 'assign'
          var: 'sum'
          val:
            type: 'expr'
            e1: type: 'valof', var: 'sum'
            op: '+'
            e2: type: 'valof', var: 'x'
        s2:
          type: 'assign'
          var: 'x'
          val: type: 'expr', e1: {type: 'valof', var: 'x'}, op: '-', e2: 1

  # TODO: more complex tests
