%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_global_stores).

%% API
-export([init/0,
         start_time/0,
         get_mfa/1,
         set_mfa/2,
         get_val/1,
         set_val/2,
         get_varname/0,
         dict_new/0,
         dict_count/1,
         dict_set/3,
         dict_get/2,
         dict_rm/2,
         dict_fold/3,
         dict_keys/1,
         dict_values/1]).

%% Macros
-define(FUNCTIONS_STORE_NAME, '_kl_funs_store').
-define(VALUES_STORE_NAME, '_kl_values_store').
-define(VARIABLE_COUNTER_STORE_NAME, '_kl_variables_counter_store').

%% Keys
-define(VARIABLE_COUNTER_KEY, var_counter).
-define(START_TIME_KEY, '__kl_start_time').

-define(PORT_KL_MODS, [shen_erl_kl_primitives,
                       shen_erl_kl_extensions]).
-define(PORT_KL_NON_OVERRIDABLE_MODS, [shen_erl_kl_overrides]).

-opaque dict() :: ets:tid().

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
  ets:new(?FUNCTIONS_STORE_NAME, [set, named_table]),
  ets:new(?VALUES_STORE_NAME, [set, named_table]),
  ets:new(?VARIABLE_COUNTER_STORE_NAME, [set, named_table]),
  set_val(?START_TIME_KEY, calendar:datetime_to_gregorian_seconds(calendar:universal_time())),
  [[set_mfa(FunName, {Mod, FunName, Arity}, true) ||
     {FunName, Arity} <- Mod:module_info(exports),
     FunName =/= module_info] || Mod <- ?PORT_KL_MODS],
  [[set_mfa(FunName, {Mod, FunName, Arity}, false) ||
     {FunName, Arity} <- Mod:module_info(exports),
     FunName =/= module_info] || Mod <- ?PORT_KL_NON_OVERRIDABLE_MODS],
  ok.

-spec start_time() -> non_neg_integer().
start_time() ->
  {ok, StartTime} = get_val(?START_TIME_KEY),
  StartTime.

-spec get_mfa(atom()) -> {ok, mfa()} | not_found.
get_mfa(FunName) ->
  case lookup(?FUNCTIONS_STORE_NAME, FunName) of
    {ok, {MFA, _IsOverridable}} -> {ok, MFA};
    not_found -> not_found
  end.

-spec set_mfa(atom(), mfa()) -> ok.
set_mfa(FunName, MFA) ->
  set_mfa(FunName, MFA, true),
  ok.

-spec get_val(atom()) -> {ok, term()} | not_found.
get_val(Key) ->
  lookup(?VALUES_STORE_NAME, Key).

-spec set_val(atom(), term()) -> ok.
set_val(Key, Val) ->
  ets:insert(?VALUES_STORE_NAME, {Key, Val}),
  ok.

-spec get_varname() -> non_neg_integer().
get_varname() ->
  Counter = lookup(?VARIABLE_COUNTER_STORE_NAME, ?VARIABLE_COUNTER_KEY, 1),
  ets:insert(?VARIABLE_COUNTER_STORE_NAME, {?VARIABLE_COUNTER_KEY, Counter + 1}),
  Counter.

-spec dict_new() -> dict().
dict_new() ->
  NewCounter = get_varname(),
  DictName = list_to_atom("DICT" ++ integer_to_list(NewCounter)),
  ets:new(DictName, [set, named_table]),
  DictName.

-spec dict_count(dict()) -> non_neg_integer().
dict_count(Dict) ->
  proplists:get_value(size, ets:info(Dict)).

-spec dict_set(dict(), term(), term()) -> ok.
dict_set(Dict, Key, Val) ->
  ets:insert(Dict, {Key, Val}).

-spec dict_get(dict(), term()) -> {ok, term()} | not_found.
dict_get(Dict, Key) ->
  lookup(Dict, Key).

-spec dict_rm(dict(), term()) -> ok.
dict_rm(Dict, Key) ->
  ets:delete(Dict, Key).

-spec dict_fold(dict(), fun((term(), term()) -> term()), term()) -> term().
dict_fold(Dict, F, Init) ->
  ets:foldl(F, Init, Dict).

-spec dict_keys(dict()) -> [term()].
dict_keys(Dict) ->
  [K || {K, _V} <- ets:tab2list(Dict)].

-spec dict_values(dict()) -> [term()].
dict_values(Dict) ->
  [V || {_K, V} <- ets:tab2list(Dict)].

%%%===================================================================
%%% Internal functions
%%%===================================================================

lookup(Tab, Key) ->
  case lookup(Tab, Key, not_found) of
    not_found -> not_found;
    Val -> {ok, Val}
  end.

lookup(Tab, Key, Default) ->
  try
    ets:lookup(Tab, Key)
  of
    [{Key, Val}] -> Val;
    [] -> Default
  catch
    error:badarg -> Default
  end.

set_mfa(FunName, MFA, false) ->
  ets:insert(?FUNCTIONS_STORE_NAME, {FunName, {MFA, false}});
set_mfa(FunName, MFA, true) ->
  case lookup(?FUNCTIONS_STORE_NAME, FunName) of
    {ok, {_MFA, false}} -> ok;
    {ok, {_MFA, true}} -> ets:insert(?FUNCTIONS_STORE_NAME, {FunName, {MFA, true}});
    not_found -> ets:insert(?FUNCTIONS_STORE_NAME, {FunName, {MFA, true}})
  end.
