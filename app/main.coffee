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


currStateBox = $ '#current-state'

# Fill evaluation output box
showState = (idx) ->
  if idx is 'last'
    idx = @states.length - 1

  currIdx = +currStateBox.val() - 1
  if idx is 'prev'
    idx = currIdx - 1
  if idx is 'next'
    idx = currIdx + 1

  # Update if set programatically
  if +currStateBox.val() isnt idx+1
    currStateBox.val idx+1

  $ '#evaluation-output'
    .html jsonViewer @states[idx]


# React to input box change
currStateBox
  .change ->
    nr = currStateBox.val()
    showState +nr - 1

$('#prev-state').click -> showState('prev')
$('#next-state').click -> showState('next')


setupStates = (parsed) ->
  @states = evaluate parsed
  $ '#nr-states'
    .text @states.length
  $ '#current-state'
    .attr max: @states.length

  # Initially, show the final state
  showState 'last'



# React to program input box change
parseProgram = ->
  program = $('#program-input').val()
  parsed = parse program
  $ '#parse-output'
    .html jsonViewer parsed
  # TODO: catch error and show line number

  # Make evaluation box react
  setupStates parsed

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
