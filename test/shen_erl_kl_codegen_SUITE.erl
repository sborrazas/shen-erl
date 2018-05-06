-module(shen_erl_kl_codegen_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         t_compile_xor/1,
         t_compile_sum/1,
         t_compile_mult/1,
         t_compile_lambda_app/1,
         t_compile_var_app/1,
         t_compile_mod_fun_app/1,
         t_compile_external_fun_app/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common test
%%%===================================================================

groups() ->
  [{compile,
    [],
    [t_compile_xor,
     t_compile_sum,
     t_compile_mult,
     t_compile_lambda_app,
     t_compile_var_app,
     t_compile_mod_fun_app,
     t_compile_external_fun_app]}].

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

%% Boolean operators
t_compile_xor(_Config) ->
  Xor = [defun, 'xor', ['X', 'Y'],
         ['and', ['or', 'X', 'Y'], ['not', ['and', 'X', 'Y']]]],
  compile_and_load(testmod, [Xor]),
  false = testmod:'xor'(false, false),
  true = testmod:'xor'(true, false),
  true = testmod:'xor'(false, true),
  false = testmod:'xor'(true, true).

%% Numeric operators
t_compile_sum(_Config) ->
  Sum = [defun, 'sum', ['X', 'Y'], ['+', 'X', 'Y']],
  compile_and_load(testmod, [Sum]),
  2 = testmod:sum(1, 1),
  3.6 = testmod:sum(2, 1.6),
  3.6 = testmod:sum(1.6, 2),
  4.4 = testmod:sum(2.2, 2.2).

t_compile_mult(_Config) ->
  Mult = [defun, 'mult', ['X', 'Y'], ['*', 'X', 'Y']],
  compile_and_load(testmod, [Mult]),
  1 = testmod:mult(1, 1),
  3.2 = testmod:mult(2, 1.6),
  3.2 = testmod:mult(1.6, 2),
  4.4 = testmod:mult(2.2, 2.0).

%% Variables and function applications
t_compile_lambda_app(_Config) ->
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], [PlusTwo, [PlusTwo, 'X']]],
  compile_and_load(testmod, [PlusFour]),
  2 = testmod:plusfour(-2),
  4 = testmod:plusfour(0),
  6 = testmod:plusfour(2).

t_compile_var_app(_Config) ->
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['let', 'P2', PlusTwo, ['P2', ['P2', 'X']]]],
  compile_and_load(testmod, [PlusFour]),
  2 = testmod:plusfour(-2),
  4 = testmod:plusfour(0),
  6 = testmod:plusfour(2).

t_compile_mod_fun_app(_Config) ->
  PlusTwo = [defun, 'plustwo', ['X'], ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['plustwo', ['plustwo', 'X']]],
  compile_and_load(testmod, [PlusTwo, PlusFour]),
  2 = testmod:plusfour(-2),
  4 = testmod:plusfour(0),
  6 = testmod:plusfour(2).

t_compile_external_fun_app(_Config) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_and_load(Mod, Funs) ->
  {ok, Mod, Bin} = shen_erl_kl_codegen:compile_module(Mod, Funs),
  code:load_binary(Mod, [], Bin).
