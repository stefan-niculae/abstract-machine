$ = require 'jquery'
JSONFormatter = require 'json-formatter-js'

parse = require '../src/parser'
{evaluate} = require '../src/evaluator'



EXAMPLES = [
  """
  x := 0;
  if !x < 1 then
    y := !x
  else
    y := 1
  """,

  """
  while 0 <= !x do {
    sum := !sum + !x;
    x := !x - 1
  }
  """
]

# Wrapper
jsonViewer = (obj) ->
  formatter = new JSONFormatter obj, openedLevels=2, hoverPreviewEnabled: yes
  formatter.render()


showState = (nr) ->
  states = evaluate @parsed
  if nr is 'last'
    nr = states.length - 1

  console.log states[nr]
  $ '#evaluation-output'
    .html jsonViewer states[nr]



# React to program input box change
parseProgram = ->
  program = $('#program-input').val()
  @parsed = parse program
  $ '#parse-output'
    .html jsonViewer parsed
  # TODO: catch error and show line number

  # Show last state
  showState 'last'

# Set event listener
$ '#program-input'
  .keyup -> parseProgram()


# Fill program input box
loadExample = (nr) ->
  $ '#program-input'
    .text EXAMPLES[nr]
  parseProgram()


# List input examples
for program, idx in EXAMPLES
  $ '<a>'
    .text "example #{idx + 1}"
    .attr href: '#'
    .click do (idx) -> -> loadExample idx  # FIXME: refactor into shorter version
    .appendTo '#examples'

# Load an initial example
loadExample 0
