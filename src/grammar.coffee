# Generated automatically by nearley
# http://github.com/Hardmath123/nearley
do ->
  id = (d)->d[0]

  isKeyword = (x) -> x in ['true', 'false', 'if', 'then', 'else', 'while', 'do'] 
  strip = (d) -> d.filter((x) -> x != null) 
  striphead = (d) -> strip(d)[0] 
  grammar = {
    ParserRules: [
          {"name": "Main", "symbols": ["_", "Stmt", "_"], "postprocess": striphead},
          {"name": "Main", "symbols": ["_", "Expr", "_"], "postprocess": striphead},
          {"name": "Main", "symbols": ["_", "Cond", "_"], "postprocess": striphead},
          {"name": "Stmt$string$1", "symbols": [{"literal":"("}, {"literal":")"}], "postprocess": (d) -> d.join('')},
          {"name": "Stmt", "symbols": ["Stmt$string$1"], "postprocess": id},
          {"name": "Stmt$string$2", "symbols": [{"literal":":"}, {"literal":"="}], "postprocess": (d) -> d.join('')},
          {"name": "Stmt", "symbols": ["Var", "_", "Stmt$string$2", "_", "Expr"], "postprocess": (d) -> type:'assign', var: d[0], val: d[4]},
          {"name": "Stmt", "symbols": ["Stmt", "_", {"literal":";"}, "_", "Stmt"], "postprocess": (d) -> type:'seq',    s1:  d[0], s2:  d[4]},
          {"name": "Stmt$string$3", "symbols": [{"literal":"i"}, {"literal":"f"}], "postprocess": (d) -> d.join('')},
          {"name": "Stmt$string$4", "symbols": [{"literal":"t"}, {"literal":"h"}, {"literal":"e"}, {"literal":"n"}], "postprocess": (d) -> d.join('')},
          {"name": "Stmt$string$5", "symbols": [{"literal":"e"}, {"literal":"l"}, {"literal":"s"}, {"literal":"e"}], "postprocess": (d) -> d.join('')},
          {"name": "Stmt", "symbols": ["Stmt$string$3", "__", "Cond", "__", "Stmt$string$4", "__", "Body", "__", "Stmt$string$5", "__", "Body"], "postprocess": (d) -> type:'if',     cond:d[2], st:  d[6], sf:d[10]},
          {"name": "Stmt$string$6", "symbols": [{"literal":"w"}, {"literal":"h"}, {"literal":"i"}, {"literal":"l"}, {"literal":"e"}], "postprocess": (d) -> d.join('')},
          {"name": "Stmt$string$7", "symbols": [{"literal":"d"}, {"literal":"o"}], "postprocess": (d) -> d.join('')},
          {"name": "Stmt", "symbols": ["Stmt$string$6", "__", "Cond", "__", "Stmt$string$7", "__", "Body"], "postprocess": (d) -> type:'while',  cond:d[2], body:d[6]},
          {"name": "Body", "symbols": ["Stmt"], "postprocess": id},
          {"name": "Body", "symbols": [{"literal":"{"}, "_", "Stmt", "_", {"literal":"}"}], "postprocess": (d) -> d[2]},
          {"name": "Expr", "symbols": ["Nr"], "postprocess": id},
          {"name": "Expr", "symbols": [{"literal":"!"}, "_", "Var"], "postprocess": (d) -> type:'valof',  var:d[2]},
          {"name": "Expr", "symbols": ["Expr", "_", "IOp", "_", "Expr"], "postprocess": (d) -> type:'expr',   e1: d[0],  op:d[2], e2:d[4]},
          {"name": "Expr", "symbols": [{"literal":"("}, "_", "Expr", "_", {"literal":")"}], "postprocess": (d) -> return d[2]},
          {"name": "Cond", "symbols": ["Bl"], "postprocess": id},
          {"name": "Cond", "symbols": ["Expr", "_", "BOp", "_", "Expr"], "postprocess": (d) -> type:'cond',   e1:d[0],   op:d[2], e2:d[4]},
          {"name": "IOp", "symbols": [{"literal":"*"}], "postprocess": id},
          {"name": "IOp", "symbols": [{"literal":"/"}], "postprocess": id},
          {"name": "IOp", "symbols": [{"literal":"+"}], "postprocess": id},
          {"name": "IOp", "symbols": [{"literal":"-"}], "postprocess": id},
          {"name": "BOp$string$1", "symbols": [{"literal":"="}, {"literal":"="}], "postprocess": (d) -> d.join('')},
          {"name": "BOp", "symbols": ["BOp$string$1"], "postprocess": id},
          {"name": "BOp$string$2", "symbols": [{"literal":"/"}, {"literal":"="}], "postprocess": (d) -> d.join('')},
          {"name": "BOp", "symbols": ["BOp$string$2"], "postprocess": id},
          {"name": "BOp", "symbols": [{"literal":"<"}], "postprocess": id},
          {"name": "BOp", "symbols": [{"literal":">"}], "postprocess": id},
          {"name": "BOp$string$3", "symbols": [{"literal":"<"}, {"literal":"="}], "postprocess": (d) -> d.join('')},
          {"name": "BOp", "symbols": ["BOp$string$3"], "postprocess": id},
          {"name": "BOp$string$4", "symbols": [{"literal":">"}, {"literal":"="}], "postprocess": (d) -> d.join('')},
          {"name": "BOp", "symbols": ["BOp$string$4"], "postprocess": id},
          {"name": "Bl$string$1", "symbols": [{"literal":"t"}, {"literal":"r"}, {"literal":"u"}, {"literal":"e"}], "postprocess": (d) -> d.join('')},
          {"name": "Bl", "symbols": ["Bl$string$1"], "postprocess": -> true},
          {"name": "Bl$string$2", "symbols": [{"literal":"f"}, {"literal":"a"}, {"literal":"l"}, {"literal":"s"}, {"literal":"e"}], "postprocess": (d) -> d.join('')},
          {"name": "Bl", "symbols": ["Bl$string$2"], "postprocess": -> false},
          {"name": "Nr", "symbols": ["Float"], "postprocess": id},
          {"name": "Nr", "symbols": ["Int"], "postprocess": id},
          {"name": "Float", "symbols": ["Int", {"literal":"."}, "Int"], "postprocess": (d) -> parseFloat(d[0] + '.' + d[2])},
          {"name": "Int$ebnf$1", "symbols": [/[0-9]/]},
          {"name": "Int$ebnf$1", "symbols": ["Int$ebnf$1", /[0-9]/], "postprocess": (d) -> d[0].concat([d[1]])},
          {"name": "Int", "symbols": ["Int$ebnf$1"], "postprocess": (d) -> parseInt  (d[0].join(''))},
          {"name": "Var$ebnf$1", "symbols": [/[\w_]/]},
          {"name": "Var$ebnf$1", "symbols": ["Var$ebnf$1", /[\w_]/], "postprocess": (d) -> d[0].concat([d[1]])},
          {"name": "Var", "symbols": ["Var$ebnf$1"], "postprocess": (d) -> d[0].join ''},
          {"name": "__$ebnf$1", "symbols": [/[\s\\n]/]},
          {"name": "__$ebnf$1", "symbols": ["__$ebnf$1", /[\s\\n]/], "postprocess": (d) -> d[0].concat([d[1]])},
          {"name": "__", "symbols": ["__$ebnf$1"], "postprocess": -> null},
          {"name": "_$ebnf$1", "symbols": []},
          {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", /[\s\\n]/], "postprocess": (d) -> d[0].concat([d[1]])},
          {"name": "_", "symbols": ["_$ebnf$1"], "postprocess": -> null}
      ],
    ParserStart: "Main"
  }
  if typeof module != 'undefined' && typeof module.exports != 'undefined'
    module.exports = grammar;
  else
    window.grammar = grammar;
