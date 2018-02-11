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

%% API
-export([parse_tree/1]).

%% Types
-type kl_tree() :: atom() |
                   number() |
                   string() |
                   [kl_tree()].

-export_type([kl_tree/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse_tree(list()) -> {ok, kl_tree()} | {error, binary()}.
parse_tree(Tokens) ->
  case parse(Tokens) of
    {ok, Exps} -> {ok, exp_tree(Exps)};
    {error, Reason} -> {error, Reason}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% type(Token) -> element(1, Token).
% line(Token) -> element(2, Token).
value(Token) -> element(3, Token).

exp_tree([Exp | Rest]) when is_list(Exp) ->
  [[exp_tree(E) || E <- Exp] | exp_tree(Rest)];
exp_tree([Exp | Rest]) ->
  [exp_tree(Exp) | exp_tree(Rest)];
exp_tree([]) -> [];
exp_tree(Exp) -> value(Exp).
