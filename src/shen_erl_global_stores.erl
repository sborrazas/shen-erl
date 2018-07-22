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
         init_vector/1,
         set_vector_val/3,
         get_vector_val/2]).

%% Macros
-define(FUNCTIONS_STORE_NAME, '_kl_funs_store').
-define(VALUES_STORE_NAME, '_kl_values_store').
-define(VECTORS_STORE_NAME, '_kl_vectors_store').
-define(VECTOR_MAX_ID, 4294967296).

%% Types
-opaque vector() :: non_neg_integer().

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
  ets:new(?FUNCTIONS_STORE_NAME, [set, named_table]),
  ets:new(?VALUES_STORE_NAME, [set, named_table]),
  ets:new(?VECTORS_STORE_NAME, [set, named_table]).

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

-spec init_vector(non_neg_integer()) -> vector().
init_vector(Length) ->
  Id = rand_id(),
  ets:insert(?VECTORS_STORE_NAME, {Id, {Length, #{}}}),
  Id.

-spec set_vector_val(vector(), non_neg_integer(), term()) -> ok | invalid_index | not_found.
set_vector_val(Id, Index, Val) ->
  try ets:lookup(?VECTORS_STORE_NAME, Id) of
    [{Id, {Length, Vec}}] when Index < Length ->
      ets:insert(?VECTORS_STORE_NAME, {Id, {Length, Vec#{Index => Val}}}),
      ok;
    [{Id, {_Size, _Vec}}] -> invalid_index;
    [] -> not_found
  catch
    error:badarg -> not_found
  end.

-spec get_vector_val(vector(), non_neg_integer()) -> {ok, term()} | invalid_index | not_found.
get_vector_val(Id, Index) ->
  try ets:lookup(?VECTORS_STORE_NAME, Id) of
    [{Id, {Length, Vec}}] when Index < Length ->
      case maps:get(Index, Vec, undefined) of
        undefined -> invalid_index;
        Val -> {ok, Val}
      end;
    [{Id, {_Length, _Vec}}] -> invalid_index;
    [] -> not_found
  catch
    error:badarg -> not_found
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rand_id() ->
  rand:uniform(?VECTOR_MAX_ID).
