nearley = require 'nearley'
grammar = require './grammar'


parse = (input) ->
  parser = new nearley.Parser(grammar.ParserRules, grammar.ParserStart)
  parser.feed input
  return parser.results[0]


module.exports = parse