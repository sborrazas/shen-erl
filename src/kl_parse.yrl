Nonterminals
  expr_list expr list.

Terminals
  symbol number string '(' ')'.

Rootsymbol expr_list.

expr_list -> expr expr_list : ['$1' | '$2'].
expr_list -> expr : ['$1'].

expr -> list : '$1'.
expr -> number : '$1'.
expr -> string : '$1'.
expr -> symbol : '$1'.

list -> '(' ')' : [].
list -> '(' expr_list ')' : '$2'.

Erlang code.
