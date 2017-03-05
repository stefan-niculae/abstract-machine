JSONFormatter = require 'json-formatter-js'
$ = require 'jquery'

OPENED_LEVELS = 2
VIEWER_CONFIG =
  hoverPreviewEnabled: yes

EXAMPLES = [
  """
  x := 0;
  if x < 1 then
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

#obj = ans:42
#formatter = new JSONFormatter obj, OPENED_LEVELS, VIEWER_CONFIG
#document.body.appendChild formatter.render()

loadExample = (nr) ->
  $ '#program-input'
    .text EXAMPLES[nr]


for idx, program of EXAMPLES
  console.log idx+1
  $ '<a>'
    .text "example #{parseInt(idx) + 1}"
    .attr href: '#'
    .click -> loadExample idx
    .appendTo '#examples'

loadExample 0

