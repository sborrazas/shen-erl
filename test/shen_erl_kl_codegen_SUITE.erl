-module(shen_erl_kl_codegen_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
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
         t_compile_static_app_more_params/1,
         t_compile_trap_error/1,
         t_compile_factorized_fun/1]).

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
     t_compile_static_app_more_params,
     t_compile_trap_error,
     t_compile_factorized_fun]}].

suite() ->
  [{timetrap, {minutes, 1}}].

all() ->
  [{group, compile}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  shen_erl_global_stores:init(),
  Config.

end_per_testcase(_Case, _Config) ->
  ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

%% Boolean operators
t_compile_xor(_Config) ->
  Xor = [defun, 'xor', ['X', 'Y'],
         ['and', ['or', 'X', 'Y'], ['not', ['and', 'X', 'Y']]]],
  compile_and_load([Xor]),
  false = kl:'xor'(false, false),
  true = kl:'xor'(true, false),
  true = kl:'xor'(false, true),
  false = kl:'xor'(true, true).

%% Numeric operators
t_compile_sum(_Config) ->
  Sum = [defun, 'sum', ['X', 'Y'], ['+', 'X', 'Y']],
  compile_and_load([Sum]),
  2 = kl:sum(1, 1),
  3.6 = kl:sum(2, 1.6),
  3.6 = kl:sum(1.6, 2),
  4.4 = kl:sum(2.2, 2.2).

t_compile_mult(_Config) ->
  Mult = [defun, 'mult', ['X', 'Y'], ['*', 'X', 'Y']],
  compile_and_load([Mult]),
  1 = kl:mult(1, 1),
  3.2 = kl:mult(2, 1.6),
  3.2 = kl:mult(1.6, 2),
  4.4 = kl:mult(2.2, 2.0).

%% if
t_compile_if(_Config) ->
  Max = [defun, 'max', ['X', 'Y'], ['if', ['>', 'X', 'Y'], 'X', 'Y']],
  compile_and_load([Max]),
  1 = kl:max(1, 1),
  2 = kl:max(2, 1.6),
  2 = kl:max(1.6, 2),
  2.2 = kl:max(2.2, 2.0).

%% freeze
t_compile_freeze(_Config) ->
  Lazy = [defun, 'lazy', ['X'], [freeze, ['+', 1, 2]]],
  compile_and_load([Lazy]),
  3 = (kl:lazy(1))().

%% Variables and function applications
t_compile_dynamic_app_lambda(_Config) ->
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], [PlusTwo, [PlusTwo, 'X']]],
  compile_and_load([PlusFour]),
  2 = kl:plusfour(-2),
  4 = kl:plusfour(0),
  6 = kl:plusfour(2).

t_compile_dynamic_app_var(_Config) ->
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['let', 'P2', PlusTwo, ['P2', ['P2', 'X']]]],
  compile_and_load([PlusFour]),
  2 = kl:plusfour(-2),
  4 = kl:plusfour(0),
  6 = kl:plusfour(2).

t_compile_dynamic_app_var_with_external_fun(_Config) -> % Variable takes precedence
  P2Defun = [defun, 'P2', ['Z'], [10]],
  PlusTwo = [lambda, 'X', ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['let', 'P2', PlusTwo, ['P2', ['P2', 'X']]]],
  compile_and_load([P2Defun, PlusFour]),
  2 = kl:plusfour(-2),
  4 = kl:plusfour(0),
  6 = kl:plusfour(2).

t_compile_dynamic_app_var_freeze(_Config) ->
  XPlusTwoLazy = [freeze, ['+', 'X', 2]],
  PlusTwo = [defun, 'plustwo', ['X'], ['let', 'XP2', XPlusTwoLazy, ['XP2']]],
  compile_and_load([PlusTwo]),
  0 = kl:plustwo(-2),
  2 = kl:plustwo(0),
  4 = kl:plustwo(2).

t_compile_static_app(_Config) ->
  PlusTwo = [defun, 'plustwo', ['X'], ['+', 'X', 2]],
  PlusFour = [defun, 'plusfour', ['X'], ['plustwo', ['plustwo', 'X']]],
  compile_and_load([PlusTwo, PlusFour]),
  2 = kl:plusfour(-2),
  4 = kl:plusfour(0),
  6 = kl:plusfour(2).

t_compile_static_app_less_params(_Config) ->
  SumCurry = [defun, 'sumcurry', ['X', 'Y'], [['+', 'X'], 'Y']],
  compile_and_load([SumCurry]),
  3 = kl:sumcurry(1, 2),
  0 = kl:sumcurry(-1, 1).

t_compile_static_app_more_params(_Config) ->
  Sum = [lambda, 'A', [lambda, 'B', ['+', 'A', 'B']]],
  WeirdSum = [defun, 'sum', ['X', 'Y'], ['if', true, Sum, 3, 'X', 'Y']],
  compile_and_load([WeirdSum]),
  3 = kl:sum(1, 2),
  0 = kl:sum(-1, 1).

t_compile_trap_error(_Config) ->
  Trapper = [defun, 'trapper', [], ['trap-error', ['/', 1, 0], [lambda, 'E', a]]],
  compile_and_load([Trapper]),
  a = kl:trapper().

t_compile_factorized_fun(_Config) ->
  %% (define factorized
  %%   [1 X | Xs] 1 -> X
  %%   [1 X | Xs] 2 -> Xs
  %%   [2 X | Xs] _ -> X)
  FactorizedFun =
    [defun, 'factorized', ['V1345', 'V1346'],
     ['%%let-label', ['%%label1347'], ['%%return', ['shen.f_error', 'factorized']],
      ['if', ['cons?', 'V1345'],
      ['let', 'V1345/hd', [hd, 'V1345'],
        ['let', 'V1345/tl', [tl, 'V1345'],
          ['%%let-label', ['%%label1348', 'V1345/hd', 'V1345/tl'],
           ['if', ['and', ['=', 2, 'V1345/hd'], ['cons?', 'V1345/tl']],
            ['%%return', [hd, 'V1345/tl']],
            ['%%goto-label', '%%label1347']],
           ['if', ['and', ['=', 1, 'V1345/hd'], ['cons?', 'V1345/tl']],
            ['if', ['=', 1, 'V1346'],
             ['%%return', [hd, 'V1345/tl']],
             ['if', ['=', 2, 'V1346'],
              ['%%return', [tl, 'V1345/tl']],
              ['%%goto-label', '%%label1348', 'V1345/hd', 'V1345/tl']]],
            ['%%goto-label', '%%label1348', 'V1345/hd', 'V1345/tl']]]]],
       ['%%goto-label', '%%label1347']]]],
  compile_and_load([FactorizedFun]),
  8 = kl:factorized({cons, 1, {cons, 8, []}}, 1),
  {cons, 9, []} = kl:factorized({cons, 1, {cons, 8, {cons, 9, []}}}, 2),
  10 = kl:factorized({cons, 2, {cons, 10, []}}, 3).

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_and_load(Funs) ->
  shen_erl_kl_codegen:load_defuns(kl, Funs),
  {ok, kl, Bin} = shen_erl_kl_codegen:compile(kl, Funs, ok),
  code:load_binary(kl, [], Bin).
