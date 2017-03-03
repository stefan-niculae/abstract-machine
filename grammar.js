// Generated automatically by nearley
// http://github.com/Hardmath123/nearley
(function () {
function id(x) {return x[0]; }
var grammar = {
    ParserRules: [
    {"name": "Main", "symbols": ["Stmt"]},
    {"name": "Main", "symbols": ["Expr"]},
    {"name": "Main", "symbols": ["Cond"]},
    {"name": "Expr", "symbols": ["Nr"]},
    {"name": "Expr", "symbols": [{"literal":"!"}, "_", "Var"]},
    {"name": "Expr", "symbols": ["Expr", "_", "IOp", "_", "Expr"]},
    {"name": "Cond", "symbols": ["Bl"]},
    {"name": "Cond", "symbols": ["Expr", "_", "BOp", "_", "Expr"]},
    {"name": "Stmt$string$1", "symbols": [{"literal":"("}, {"literal":")"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Stmt", "symbols": ["Stmt$string$1"]},
    {"name": "Stmt$string$2", "symbols": [{"literal":":"}, {"literal":"="}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Stmt", "symbols": ["Var", "_", "Stmt$string$2", "_", "Expr"]},
    {"name": "Stmt", "symbols": ["Stmt", "_", {"literal":";"}, "_", "Stmt"]},
    {"name": "Stmt$string$3", "symbols": [{"literal":"i"}, {"literal":"f"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Stmt$string$4", "symbols": [{"literal":"t"}, {"literal":"h"}, {"literal":"e"}, {"literal":"n"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Stmt$string$5", "symbols": [{"literal":"e"}, {"literal":"l"}, {"literal":"s"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Stmt", "symbols": ["Stmt$string$3", "_", "Cond", "_", "Stmt$string$4", "_", "Stmt", "_", "Stmt$string$5", "_", "Stmt"]},
    {"name": "Stmt$string$6", "symbols": [{"literal":"w"}, {"literal":"h"}, {"literal":"i"}, {"literal":"l"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Stmt$string$7", "symbols": [{"literal":"d"}, {"literal":"o"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Stmt", "symbols": ["Stmt$string$6", "_", "Cond", "_", "Stmt$string$7", "_", "Stmt"]},
    {"name": "IOp", "symbols": [{"literal":"+"}]},
    {"name": "IOp", "symbols": [{"literal":"-"}]},
    {"name": "IOp", "symbols": [{"literal":"/"}]},
    {"name": "IOp", "symbols": [{"literal":"*"}]},
    {"name": "IOp", "symbols": [{"literal":"%"}]},
    {"name": "BOp", "symbols": [{"literal":"="}]},
    {"name": "BOp$string$1", "symbols": [{"literal":"/"}, {"literal":"="}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "BOp", "symbols": ["BOp$string$1"]},
    {"name": "BOp", "symbols": [{"literal":"<"}]},
    {"name": "BOp", "symbols": [{"literal":">"}]},
    {"name": "BOp$string$2", "symbols": [{"literal":"<"}, {"literal":"="}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "BOp", "symbols": ["BOp$string$2"]},
    {"name": "BOp$string$3", "symbols": [{"literal":">"}, {"literal":"="}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "BOp", "symbols": ["BOp$string$3"]},
    {"name": "Var$ebnf$1", "symbols": [/[\w_]/]},
    {"name": "Var$ebnf$1", "symbols": ["Var$ebnf$1", /[\w_]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "Var", "symbols": ["Var$ebnf$1"]},
    {"name": "Bl$string$1", "symbols": [{"literal":"t"}, {"literal":"r"}, {"literal":"u"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Bl", "symbols": ["Bl$string$1"]},
    {"name": "Bl$string$2", "symbols": [{"literal":"f"}, {"literal":"a"}, {"literal":"l"}, {"literal":"s"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Bl", "symbols": ["Bl$string$2"]},
    {"name": "Nr", "symbols": ["Float"]},
    {"name": "Nr", "symbols": ["Int"]},
    {"name": "Float", "symbols": ["Int", {"literal":"."}, "Int"], "postprocess": function(d) {return {v:parseFloat(d[0].v + d[1].v + d[2].v)}}},
    {"name": "Float", "symbols": ["Int"], "postprocess": function(d) {return {v:parseInt(d[0].v)}}},
    {"name": "Int$ebnf$1", "symbols": [/[0-9]/]},
    {"name": "Int$ebnf$1", "symbols": ["Int$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "Int", "symbols": ["Int$ebnf$1"], "postprocess": function(d) {return {v:d[0].join("")}}},
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", /[\s]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "_", "symbols": ["_$ebnf$1"], "postprocess": function(d) {return null }}
]
  , ParserStart: "Main"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
