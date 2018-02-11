-module(shen_erl_kl_codegen_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         t_compile/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common test
%%%===================================================================

groups() ->
  [{compile,
    [],
    [t_compile]}].

suite() ->
  [{timetrap, {minutes, 1}}].

all() ->
  [{group, compile}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

t_compile(Config) ->
  Config.
