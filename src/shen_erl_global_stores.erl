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
         get_varname/0]).

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
