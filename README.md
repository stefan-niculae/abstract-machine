# abstract-machine

Control stack, Work stack, Memory model of execution.

For a triplet `<c, s, m>` we process commands in `c` one by one.
We use the working stack `s` for evaluating expressions, conditions etc.
The memory `m` holds the values of variables.


The syntax & rules are defined in `docs/definition`.
The syntax is implemented in `src/grammar` and tested by `tests/syntax`.
The semantics are implemented in `src/evaluator` and tested by `tests/semantics`.
The types are defined in `src/types`.
The app behaviour lies in 'app/interface', the layout in 'app/page' and the form in 'app/style'.

## Demo
Online: http://stefan1niculae.github.io/abstract-machine



<p align="center">

  <img src="https://github.com/stefan1niculae/abstract-machine/raw/master/app/demo.gif" alt="Demo Gif"/>

</p>

Inspect the results of parsing your program and its configuration state after each step of execution.
Provides error messages and guards against infinite cycles and division by zero.


## Build
- Download dependencies: `npm install`
- Fix nearley script: `curl https://raw.githubusercontent.com/Hardmath123/nearley/master/lib/nearley.js > node_modules/nearley/lib/nearley.js`
- Bundle scripts & stylesheet: `node_modules/.bin/webpack`


## Dev
- Generate grammar: `node_modules/.bin/nearleyc src/grammar.ne > src/grammar.coffee`
- Compile CoffeeScript: `node_modules/.bin/coffee --compile --map src app`
- Compile Sass: `node_modules/.bin/node-sass app --output app --source-map true`

Nearley playground: https://omrelli.ug/nearley-playground


## Test
- `node_modules/.bin/jasmine tests/syntax.js tests/semantics.js tests/programs.js`


## Run
Open `app/page.html`


## Bugs
- Order of operations: `4 + 6/2` evaluates to `(4+6) / 2` (workaround: `4 + (6/2)` to get `7`)
- Keywords: the following are treated as a variable name: `true, false, if, then, else, while, do, break, continue, exit`


## TODO
Next:
- explanation for each evaluation step


Future:
- make remove trailing bang from `break! continue! exit!`
- negative number literals
- boolean variables
- logical expressions (and, or, xor, not)
- unary operators (++, -- etc)
- more arithmetic operators (%, ^, +=, -= etc)
- editor numbers & syntax highlighting (codemirror & coffeescript syntax)
- more helpful error messages (including line number)
- prettier visualization
- newline for sequencing
- comments
- functions
