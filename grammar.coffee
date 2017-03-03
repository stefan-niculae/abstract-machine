# Generated automatically by nearley
# http://github.com/Hardmath123/nearley
do ->
  id = (d)->d[0]

  grammar = {
    ParserRules: [
          {"name": "main", "symbols": [{"literal":"x"}], "postprocess": (d) -> return 2}
      ],
    ParserStart: "main"
  }
  if typeof module != 'undefined' && typeof module.exports != 'undefined'
    module.exports = grammar;
  else
    window.grammar = grammar;
