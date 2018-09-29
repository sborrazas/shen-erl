%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_global_stores).

%% API
-export([init/0,
         get_mfa/1,
         set_mfa/2,
         get_val/1,
         set_val/2,
         get_varname/0]).

%% Macros
-define(FUNCTIONS_STORE_NAME, '_kl_funs_store').
-define(VALUES_STORE_NAME, '_kl_values_store').
-define(VARIABLE_COUNTER_STORE_NAME, '_kl_variables_counter_store').
-define(VARIABLE_COUNTER_KEY, var_counter).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
  ets:new(?FUNCTIONS_STORE_NAME, [set, named_table]),
  ets:new(?VALUES_STORE_NAME, [set, named_table]),
  ets:new(?VARIABLE_COUNTER_STORE_NAME, [set, named_table]).

-spec get_mfa(atom()) -> {ok, mfa()} | not_found.
get_mfa(FunName) ->
  try ets:lookup(?FUNCTIONS_STORE_NAME, FunName) of
    [{FunName, Arity}] -> {ok, Arity};
    [] -> not_found
  catch
    error:badarg -> not_found
  end.

-spec set_mfa(atom(), mfa()) -> ok.
set_mfa(FunName, MFA) ->
  ets:insert(?FUNCTIONS_STORE_NAME, {FunName, MFA}).

-spec get_val(atom()) -> {ok, term()} | not_found.
get_val(Key) ->
  try ets:lookup(?VALUES_STORE_NAME, Key) of
    [{Key, Val}] -> {ok, Val};
    [] -> not_found
  catch
    error:badarg -> not_found
  end.

-spec set_val(atom(), term()) -> ok.
set_val(Key, Val) ->
  ets:insert(?VALUES_STORE_NAME, {Key, Val}).

-spec get_varname() -> non_neg_integer().
get_varname() ->
  Counter = try
              ets:lookup(?VARIABLE_COUNTER_STORE_NAME, ?VARIABLE_COUNTER_KEY)
            of
              [{?VARIABLE_COUNTER_KEY, C}] -> C;
              [] -> 1
            catch
              error:badarg -> 1
            end,
  ets:insert(?VARIABLE_COUNTER_STORE_NAME, {?VARIABLE_COUNTER_KEY, Counter + 1}),
  Counter.

%%%===================================================================
%%% Internal functions
%%%===================================================================
