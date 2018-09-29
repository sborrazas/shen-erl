%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_env).

%% API
-export([new/0,
         fetch/2,
         store_var/2,
         new_var/1]).

%% Types
-record(env, {vars :: orddict:orddict(atom(), atom())}).

-opaque env() :: #env{}.

-export_type([env/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> env().
new() ->
  #env{vars = orddict:new()}.

-spec fetch(env(), atom()) -> {ok, atom()} | not_found.
fetch(#env{vars = Vars}, VarKey) ->
  case orddict:find(VarKey, Vars) of
    {ok, Value} -> {ok, Value};
    error -> not_found
  end.

-spec store_var(env(), atom()) -> {atom(), env()}.
store_var(Env = #env{vars = Vars}, Var) ->
  Value = list_to_atom("V" ++ integer_to_list(shen_erl_global_stores:get_varname())),
  {Value, Env#env{vars = orddict:store(Var, Value, Vars)}}.

-spec new_var(env()) -> {atom(), env()}.
new_var(Env = #env{vars = Vars}) ->
  Value = list_to_atom("V" ++ integer_to_list(shen_erl_global_stores:get_varname())),
  Vars2 = orddict:store(Value, Value, Vars),
  {Value, Env#env{vars = Vars2}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
