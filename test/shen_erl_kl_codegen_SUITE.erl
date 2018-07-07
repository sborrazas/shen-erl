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
         t_compile_dynamic_app_lambda/1,
         t_compile_dynamic_app_var/1,
         t_compile_dynamic_app_var_with_external_fun/1,
         t_compile_dynamic_app_var_freeze/1,
         t_compile_static_app/1,
         t_compile_static_app_less_params/1,
         t_compile_static_app_more_params/1]).

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
     t_compile_dynamic_app_lambda,
     t_compile_dynamic_app_var,
     t_compile_dynamic_app_var_with_external_fun,
     t_compile_dynamic_app_var_freeze,
     t_compile_static_app,
     t_compile_static_app_less_params,
     t_compile_static_app_more_params]}].

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

%% if
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
t_compile_dynamic_app_lambda(_Config) ->
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], [PlusTwo, [PlusTwo, 'X']]],
  compile_and_load([PlusFour]),
  2 = plusfour:plusfour(-2),
  4 = plusfour:plusfour(0),
  6 = plusfour:plusfour(2).

t_compile_dynamic_app_var(_Config) ->
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['let', 'P2', PlusTwo, ['P2', ['P2', 'X']]]],
  compile_and_load([PlusFour]),
  2 = plusfour:plusfour(-2),
  4 = plusfour:plusfour(0),
  6 = plusfour:plusfour(2).

t_compile_dynamic_app_var_with_external_fun(_Config) -> % Variable takes precedence
  P2Defun = [defun, 'P2', ['Z'], [10]],
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['let', 'P2', PlusTwo, ['P2', ['P2', 'X']]]],
  compile_and_load([P2Defun, PlusFour]),
  2 = plusfour:plusfour(-2),
  4 = plusfour:plusfour(0),
  6 = plusfour:plusfour(2).

t_compile_dynamic_app_var_freeze(_Config) ->
  XPlusTwoLazy = [freeze, ['+', 'X', 2]],
  PlusTwo = [defun, 'plustwo', ['X'], ['let', 'XP2', XPlusTwoLazy, ['XP2']]],
  compile_and_load([PlusTwo]),
  0 = plustwo:plustwo(-2),
  2 = plustwo:plustwo(0),
  4 = plustwo:plustwo(2).

t_compile_static_app(_Config) ->
  PlusTwo = [defun, 'plustwo', ['X'], ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['plustwo', ['plustwo', 'X']]],
  compile_and_load([PlusTwo, PlusFour]),
  2 = plusfour:plusfour(-2),
  4 = plusfour:plusfour(0),
  6 = plusfour:plusfour(2).

t_compile_static_app_less_params(_Config) ->
  SumCurry = [defun, 'sumcurry', ['X', 'Y'], [['+', 'X'], 'Y']],
  compile_and_load([SumCurry]),
  3 = sumcurry:sumcurry(1, 2),
  0 = sumcurry:sumcurry(-1, 1).

t_compile_static_app_more_params(_Config) ->
  Sum = [lambda, 'A', [lambda, 'B', ['+', 'A', 'B']]],
  WeirdSum = [defun, 'sum', ['X', 'Y'], ['if', true, Sum, 3, 'X', 'Y']],
  compile_and_load([WeirdSum]),
  3 = sum:sum(1, 2),
  0 = sum:sum(-1, 1).

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_and_load(Funs) ->
  {ok, Binaries} = shen_erl_kl_codegen:compile(Funs),
  [code:load_binary(Mod, [], Bin) || {Mod, Bin} <- Binaries].
