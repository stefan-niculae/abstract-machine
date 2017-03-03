nearley = require './nearley'
grammar = require './grammar'

parser = new nearley.Parser(grammar.ParserRules, grammar.ParserStart)
parser.feed("x")
console.log parser.results
