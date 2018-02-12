%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_env).

%% API
-export([new/0,
         fetch/2,
         new_var/2]).

%% Types

-record(env, {vars :: orddict:orddict(atom(), atom()),
              funs :: #{},
              var_count :: non_neg_integer()}).

-opaque env() :: #env{}.

-export_type([env/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> env().
new() ->
  #env{vars = orddict:new(), var_count = 1}.

-spec fetch(env(), atom()) -> {ok, atom()} | not_found.
fetch(#env{vars = Vars}, VarKey) ->
  case orddict:find(VarKey, Vars) of
    {ok, Value} -> {ok, Value};
    error -> not_found
  end.

-spec new_var(env(), atom()) -> {atom(), env()}.
new_var(Env = #env{vars = Vars, var_count = Count}, VarKey) ->
  Value = "V" ++ integer_to_list(Count),
  Vars2 = orddict:store(VarKey, Value, Vars),
  {Value, Env#env{vars = Vars2, var_count = Count + 1}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
