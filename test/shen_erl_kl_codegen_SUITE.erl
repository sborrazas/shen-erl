-module(shen_erl_kl_codegen_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         t_compile_xor/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common test
%%%===================================================================

groups() ->
  [{compile,
    [],
    [t_compile_xor]}].

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

t_compile_xor(_Config) ->
  Mod = xormod,
  Fun = 'xor',
  Xor = [defun, Fun, ['X', 'Y'],
         ['and', ['or', 'X', 'Y'], ['not', ['and', 'X', 'Y']]]],
  {ok, Mod, Bin} = shen_erl_kl_codegen:compile_module(Mod, [Xor]),
  code:load_binary(Mod, [], Bin),
  false = Mod:Fun(false, false),
  true = Mod:Fun(true, false),
  true = Mod:Fun(false, true),
  false = Mod:Fun(true, true).
