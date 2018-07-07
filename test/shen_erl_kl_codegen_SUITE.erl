-module(shen_erl_kl_codegen_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         t_compile_xor/1,
         t_compile_sum/1,
         t_compile_mult/1,
         t_compile_if/1,
         t_compile_freeze/1,
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
     t_compile_if,
     t_compile_freeze,
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
  compile_and_load([Xor]),
  false = 'xor':'xor'(false, false),
  true = 'xor':'xor'(true, false),
  true = 'xor':'xor'(false, true),
  false = 'xor':'xor'(true, true).

%% Numeric operators
t_compile_sum(_Config) ->
  Sum = [defun, 'sum', ['X', 'Y'], ['+', 'X', 'Y']],
  compile_and_load([Sum]),
  2 = sum:sum(1, 1),
  3.6 = sum:sum(2, 1.6),
  3.6 = sum:sum(1.6, 2),
  4.4 = sum:sum(2.2, 2.2).

t_compile_mult(_Config) ->
  Mult = [defun, 'mult', ['X', 'Y'], ['*', 'X', 'Y']],
  compile_and_load([Mult]),
  1 = mult:mult(1, 1),
  3.2 = mult:mult(2, 1.6),
  3.2 = mult:mult(1.6, 2),
  4.4 = mult:mult(2.2, 2.0).

% if
t_compile_if(_Config) ->
  Max = [defun, 'max', ['X', 'Y'], ['if', ['>', 'X', 'Y'], 'X', 'Y']],
  compile_and_load([Max]),
  1 = max:max(1, 1),
  2 = max:max(2, 1.6),
  2 = max:max(1.6, 2),
  2.2 = max:max(2.2, 2.0).

%% freeze
t_compile_freeze(_Config) ->
  Lazy = [defun, 'lazy', ['X'], [freeze, ['+', 1, 2]]],
  compile_and_load([Lazy]),
  3 = (lazy:lazy(1))().

%% Variables and function applications
t_compile_lambda_app(_Config) ->
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], [PlusTwo, [PlusTwo, 'X']]],
  compile_and_load([PlusFour]),
  2 = plusfour:plusfour(-2),
  4 = plusfour:plusfour(0),
  6 = plusfour:plusfour(2).

t_compile_var_app(_Config) ->
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['let', 'P2', PlusTwo, ['P2', ['P2', 'X']]]],
  compile_and_load([PlusFour]),
  2 = plusfour:plusfour(-2),
  4 = plusfour:plusfour(0),
  6 = plusfour:plusfour(2).

t_compile_mod_fun_app(_Config) ->
  PlusTwo = [defun, 'plustwo', ['X'], ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['plustwo', ['plustwo', 'X']]],
  compile_and_load([PlusTwo, PlusFour]),
  2 = plusfour:plusfour(-2),
  4 = plusfour:plusfour(0),
  6 = plusfour:plusfour(2).

t_compile_external_fun_app(_Config) ->
  PlusTwo = [defun, 'plustwo', ['X'], ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['plustwo', ['plustwo', 'X']]],
  compile_and_load([PlusTwo, PlusFour]),
  2 = plusfour:plusfour(-2),
  4 = plusfour:plusfour(0),
  6 = plusfour:plusfour(2).

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_and_load(Funs) ->
  {ok, Binaries} = shen_erl_kl_codegen:compile(Funs),
  [code:load_binary(Mod, [], Bin) || {Mod, Bin} <- Binaries].
