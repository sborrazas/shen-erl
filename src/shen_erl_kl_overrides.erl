%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_overrides).

%% API
-export(['symbol?'/1,
         'shen.dict'/1,
         'shen.dict?'/1,
         'shen.dict-count'/1,
         'shen.dict->'/3,
         'shen.<-dict'/2,
         'shen.dict-rm'/2,
         'shen.dict-fold'/3,
         'shen.dict-keys'/1,
         'shen.dict-values'/1]).

%%%===================================================================
%%% API
%%%===================================================================

'symbol?'(Val) when is_atom(Val) -> true;
'symbol?'(_Val) -> false.

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
