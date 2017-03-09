$ = require 'jquery/dist/jquery.slim'
JSONFormatter = require 'json-formatter-js'

parse = require '../src/parser'
{evaluate} = require '../src/evaluator'

require './style'



EXAMPLES =

  minimum: """
  a = 1;
  b = 24;

  if a < b then
    min = a
  else
    min = b
  """

  factorial: """
  n = 5;
  fact = 1;

  while true do {
    fact = fact * n;
    n = n - 1;
    if n == 0 then break! else ()
  }
  """

  euclid: """
  a = 18;
  b = 30;

  while a != b do
    if a > b then
      a = a - b
    else
      b = b - a
  """

  slides: """
  x = 3; sum = 0;
  while x > 0 do {
    sum = sum + x;
    x = x - 1
  }
  """


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

class Configuration  # for better readability in the viewer
  constructor: ({@c, @s, @m, terminationCause}) ->
    if terminationCause?  # only whow it if it exists
      @terminationCause = terminationCause

Number.prototype.clamp = (min, max) ->
  return min if this < min
  return max if this > max
  return this


# Fill evaluation output box
showState = (idx) ->
  lastIdx = @states.length - 1
  if idx is 'first'
    idx = 0
  if idx is 'last'
    idx = lastIdx

  currIdx = +currStateBox.val() - 1
  if idx is 'prev'
    idx = currIdx - 1
  if idx is 'next'
    idx = currIdx + 1

  idx = idx.clamp 0, lastIdx

  # Update if set programatically
  if +currStateBox.val() isnt idx+1
    currStateBox.val idx+1

  state = @states[idx]
  # Fill output box
  evalOutput
    .removeClass 'info error'
    .html jsonViewer new Configuration state


# React to input box change
currStateBox
  .change ->
    nr = currStateBox.val()
    showState +nr - 1

$('#first-state').click (e) -> e.preventDefault(); showState 'first'
$('#prev-state') .click (e) -> e.preventDefault(); showState 'prev'
$('#next-state') .click (e) -> e.preventDefault(); showState 'next'
$('#last-state') .click (e) -> e.preventDefault(); showState 'last'


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
  if /^\s*$/.test program
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
loadExample = (name) ->
  programInput.val EXAMPLES[name]
  parseProgram()


# List input examples
for name, program of EXAMPLES
  $ '<a>'
    .text name
    .attr href: '#'
    .click do (name) -> -> loadExample name  # FIXME: refactor into shorter version
    .appendTo '#examples'

# Load an initial example
loadExample 'minimum'
