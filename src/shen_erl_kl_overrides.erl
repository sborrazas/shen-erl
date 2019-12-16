%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_overrides).

%% API
-export(['hash'/2,
         'not'/1,
         'boolean?'/1,
         'integer?'/1,
         '@p'/2,
         'symbol?'/1,
         'shen.dict'/1,
         'shen.dict?'/1,
         'shen.dict-count'/1,
         'shen.dict->'/3,
         'shen.<-dict'/2,
         'shen.dict-rm'/2,
         'shen.dict-fold'/3,
         'shen.dict-keys'/1,
         'shen.dict-values'/1,
         'read-file-as-bytelist'/1,
         'shen.read-file-as-charlist'/1,
         'read-file-as-string'/1,
         'cd'/1]).

%%%===================================================================
%%% API
%%%===================================================================

'symbol?'(Val) when is_atom(Val) -> true;
'symbol?'(_Val) -> false.

hash(Val, Bound) ->
  erlang:phash2(Val, Bound).

'not'(Val) -> not Val.

'boolean?'(Val) -> is_boolean(Val).

'integer?'(Val) -> is_integer(Val).

'@p'(X, Y) ->
  Dict = shen_erl_global_stores:dict_new(),
  shen_erl_global_stores:dict_set(Dict, [{0, 'shen.tuple'},
                                         {1, X},
                                         {2, Y}]),
  {vector, 3, Dict}.

%% Dictionary overrides
'shen.dict'(_Size) ->
  {dict, shen_erl_global_stores:dict_new()}.

'shen.dict?'({dict, _Dict}) -> true;
'shen.dict?'(_Val) -> false.

'shen.dict-count'({dict, Dict}) ->
  shen_erl_global_stores:dict_count(Dict).

'shen.dict->'({dict, Dict}, Key, Val) ->
  shen_erl_global_stores:dict_set(Dict, Key, Val),
  Val.

'shen.<-dict'({dict, Dict}, Key) ->
  case shen_erl_global_stores:dict_get(Dict, Key) of
    {ok, Val} -> Val;
    not_found ->
      {string, KeyStr} = shen_erl_primitives:str(Key),
      shen_erl_primitives:'simple-error'({string, "`" ++ KeyStr ++ "` not found."})
  end.

'shen.dict-rm'({dict, Dict}, Key) ->
  shen_erl_global_stores:dict_rm(Dict, Key),
  Key.

'shen.dict-fold'({dict, Dict}, F, Init) ->
  shen_erl_global_stores:dict_fold(Dict, F, Init).

'shen.dict-keys'({dict, Dict}) ->
  shen_erl_global_stores:dict_keys(Dict).

'shen.dict-values'({dict, Dict}) ->
  shen_erl_global_stores:dict_values(Dict).

%% Files
'read-file-as-bytelist'({string, Filename}) ->
  {ok, Binary} = file:read_file(Filename),
  binary_to_list(Binary).

'shen.read-file-as-charlist'({string, Filename}) ->
  {ok, Binary} = file:read_file(Filename),
  binary_to_list(Binary).

'read-file-as-string'(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  {string, binary_to_list(Binary)}.

'cd'({string, ""}) ->
  'cd'(shen_erl_kl_primitives:get('*home-directory*'));
'cd'(DirStr = {string, Dir}) ->
  shen_erl_kl_primitives:set('*home-directory*', DirStr),
  file:set_cwd(Dir).
