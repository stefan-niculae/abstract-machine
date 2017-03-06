$ = require 'jquery'
JSONFormatter = require 'json-formatter-js'
#CodeMirror = require 'codemirror'

parse = require '../src/parser'
{evaluate} = require '../src/evaluator'



EXAMPLES = [
  """
  outer = 0;
  while outer != 5 do {
    outer = 5;
    inner = 0;
    while inner < 2 do {
      inner = inner + 1;
      continue!;
      outer = 100
    }
  }
  """,

  """
  x = 0;
  while x < 2 do {
    x = x + 1;
    continue!;
    x = 0
  };
  y = 100
  """,

  # TODO: check binding for while .. do with no brackets
  """
  outer = 0;
  while outer < 2 do {
    while true do {
      break!
    };
    outer = outer + 1
  }
  """,

  # state 17 has the head Break
  """
  x = 0;
  while x == 0 do {
    x = 1;
    break!;
    x = 2
  };
  y = 100
  """,


  """
  a = 1;
  b = 7;
  if a < b then
    min = a
  else
    min = b
  """,

  """
  x = 3;
  sum = 0;
  while x > 0 do {
    sum = sum + x;
    x = x - 1
  }
  """
]


# Wrapper
jsonViewer = (obj) ->
  formatter = new JSONFormatter obj, openedLevels=2, hoverPreviewEnabled: yes
  formatter.render()


# Cached because they're often used
programInput = $ '#program-input'
parseOutput  = $ '#parse-output'
evalOutput   = $ '#evaluation-output'
evalControl  = $ '#evaluation-control'
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

  # Fill output box
  evalOutput
    .removeClass 'info error'
    .html jsonViewer @states[idx]
  # TODO: catch compilation error and show line number


# React to input box change
currStateBox
  .change ->
    nr = currStateBox.val()
    showState +nr - 1

$('#prev-state').click (e) ->
  e.preventDefault()
  showState('prev')
$('#next-state').click (e) ->
  e.preventDefault()
  showState('next')


# React to the new states of execution
setupStates = (parsed) ->
  try
    @states = evaluate parsed

    evalControl.show()
    $('#nr-states').text @states.length
    currStateBox.attr max: @states.length

    # Initially, show the final state
    showState 'last'

  catch err
    evalControl.hide()
    evalOutput
      .removeClass 'info'
      .addClass 'error'
      .text err.message



emptyInput = ->
  parseOutput
    .removeClass 'error'
    .addClass 'info'
    .text 'enter some code in the box above...'

  evalOutput
    .removeClass 'error'
    .addClass 'info'
    .text '...to play with its execution'
  evalControl.hide()


# React to program input box change
parseProgram = ->
  program = programInput.val()

  # Handle empty input
  if program.length == 0
    emptyInput()
    return

  try
    parsed = parse program

    if parsed is undefined
      throw new Error "Cannot parse"  # will be caught below

    parseOutput
      .removeClass 'info error'
      .html jsonViewer parsed

    # Make evaluation box react
    setupStates parsed

  catch err
    parseOutput
      .removeClass 'info'
      .addClass 'error'
      .text err.message.replace('nearley: ', '')

    evalOutput
      .removeClass 'error'
      .addClass 'info'
      .text 'fix syntax errors first'
    evalControl.hide()


# Set event listener
programInput.keyup -> parseProgram()


# Fill program input box
loadExample = (nr) ->
  programInput.val EXAMPLES[nr]
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
