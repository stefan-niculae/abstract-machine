# abstract-machine

Control stack, Work stack, Memory model of execution

TODO: simple explanation of each component & docs

Demo: TODO ghpages


## Build
- Download dependencies: `npm install`
- Fix nearley script: `curl https://raw.githubusercontent.com/Hardmath123/nearley/master/lib/nearley.js > node_modules/nearley/lib/nearley.js`
- Bundle scripts: `node_modules/.bin/webpack`


## Dev
- Generate grammar: `node_modules/.bin/nearleyc src/grammar.ne > src/grammar.coffee`
- Compile CoffeeScript: `node_modules/.bin/coffee --compile --map .`
- Compile Sass: `sass app/style.sass > app/style.css` TODO: add sass-loader to webpack

Nearley playground: https://omrelli.ug/nearley-playground


## Test
- `node_modules/.bin/jasmine tests/syntax.js tests/semantics.js`


## Run
Open `app/page.html`


## Bugs
- Order of operations: `4 + 6/2` evaluates to `(4+6) / 2`
- Workaround: `4 + (6/2)` to get `7`
- Keywords: the following are treated as a variable name: `true, false, if, then, else, while, do`


## TODO
Next:
- break, continue, exit
- editor numbers & syntax highlighting (codemirror & coffeescript syntax)
- syntax & runtime errors
- explanation for each evaluation step


Future:
- boolean variables
- prettier visualization
- newline for sequencing?
