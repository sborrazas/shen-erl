%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_env).

%% API
-export([new/0,
         fetch/2]).

%% Types

-record(env, {vars :: orddict:orddict(atom(), string()),
              funs :: #{}}).

-opaque env() :: #env{}.

-export_type([env/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> env().
new() ->
  #env{vars = orddict:new()}.

-spec fetch(env(), atom()) -> {ok, string()} | not_found.
fetch(#env{vars = Vars}, VarKey) ->
  case orddict:find(VarKey, Vars) of
    {ok, Value} -> {ok, Value};
    error -> not_found
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
