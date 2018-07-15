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
         set_val/2]).

%% Macros
-define(FUNCTIONS_STORE_NAME, '_kl_funs_store').
-define(VALUES_STORE_NAME, '_kl_values_store').

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
  ets:new(?FUNCTIONS_STORE_NAME, [set, named_table]),
  ets:new(?VALUES_STORE_NAME, [set, named_table]).

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
  ok.

-spec set_val(atom(), term()) -> ok.
set_val(Key, Val) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
