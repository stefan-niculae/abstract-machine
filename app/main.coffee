$ = require 'jquery'
parse = require '../src/parser'
JSONFormatter = require 'json-formatter-js'



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

jsonViewer = (obj) ->
  formatter = new JSONFormatter obj, openedLevels=1, hoverPreviewEnabled: yes
  formatter.render()


# React to program input box change
parseProgram = ->
  program = $('#program-input').val()
  parsed = parse program
  $ '#parse-output'
    .html jsonViewer(parsed)
  # TODO: catch error and show line number

$ '#program-input'
  .keyup -> parseProgram()


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
