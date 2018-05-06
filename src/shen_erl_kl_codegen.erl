%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_codegen).

%% API
-export([compile_module/2]).

%% Macros
-define(ERL_TRUE, erl_syntax:atom(true)).
-define(ERL_FALSE, erl_syntax:atom(false)).

%% Types
-type core_lit() :: {var, string()} |
                    {atom, atom()}.

-type core_clause() :: {atom(), core_exp(), core_exp()}.

-type core_exp() :: core_lit() |
                    {list, [core_lit()]} |
                    {'case', core_exp(), [core_clause()]}.

-type form() :: erl_syntax:syntaxTree().

-record(code, {signatures :: [form()],
               forms :: [{atom(), form()}],
               toplevel :: form()}).

-opaque code() :: #code{}.

-export_type([code/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec compile_module(atom(), shen_erl_kl_parse:kl_tree()) -> {ok, module(), binary()} |
                                                      {error, binary()}.
compile_module(ModName, ToplevelExps) ->
  #code{signatures = Signatures, forms = Forms, toplevel = _Toplevel} =
    compile_toplevel(ToplevelExps, #code{signatures = [], forms = [], toplevel = []}),

  Module = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(ModName)]),
  ModForm =  erl_syntax:revert(Module),

  Export = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list(Signatures)]),
  ExportForm = erl_syntax:revert(Export),

  io:format(standard_error, "FORMS: ~p~n", [Forms]),
  case compile:forms([ModForm, ExportForm | Forms]) of
    {ok, Mod, Bin} -> {ok, Mod, Bin};
    SomethingElse -> {error, SomethingElse}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_toplevel([[defun, Name, Args, Body] | Rest],
                 Code = #code{signatures = Signatures, forms = Forms}) ->
  Env = shen_erl_kl_env:new(),
  {ArgsCode, Env2} = fun_vars(Args, Env),
  BodyCode = compile_exp(Body, Env2),
  Clause =  erl_syntax:clause(ArgsCode, [], [BodyCode]),
  Function = erl_syntax:function(erl_syntax:atom(Name), [Clause]),
  Form = erl_syntax:revert(Function),
  Signature = erl_syntax:arity_qualifier(erl_syntax:atom(Name), erl_syntax:integer(length(Args))),
  compile_toplevel(Rest, Code#code{signatures = [Signature | Signatures], forms = [Form | Forms]});
compile_toplevel([Exp | Rest], Code = #code{toplevel = Toplevel}) ->
  ToplevelCode = compile_exp(Exp, shen_erl_kl_env:new()),
  compile_toplevel(Rest, Code#code{toplevel = Toplevel ++ ToplevelCode});
compile_toplevel([], Code = #code{signatures = Signatures, toplevel = Toplevel}) ->
  Code#code{signatures = lists:reverse(Signatures), toplevel = lists:reverse(Toplevel)}.

%% Lists
compile_exp([], _Env) -> % ()
  erl_syntax:nil();

%% Boolean operators
compile_exp(['and', Exp1, Exp2], Env) -> % (and Exp1 Exp2)
  CExp1 = compile_exp(Exp1, Env),
  CExp2 = compile_exp(Exp2, Env),
  erl_syntax:infix_expr(CExp1, erl_syntax:operator("and"), CExp2);
compile_exp(['or', Exp1, Exp2], Env) -> % (or Exp1 Exp2)
  CExp1 = compile_exp(Exp1, Env),
  CExp2 = compile_exp(Exp2, Env),
  erl_syntax:infix_expr(CExp1, erl_syntax:operator("or"), CExp2);
compile_exp(['not', Exp], Env) -> % (not Exp)
  CExp = compile_exp(Exp, Env),
  erl_syntax:prefix_expr(erl_syntax:operator("not"), CExp);

%% Symbols and variables
compile_exp(Exp, Env) when is_atom(Exp) -> % a
  case shen_erl_kl_env:fetch(Env, Exp) of
    {ok, VarName} -> erl_syntax:variable(VarName);
    not_found -> erl_syntax:atom(Exp)
  end;

%% Numbers
compile_exp(Exp, _Env) when is_integer(Exp) -> % 1
  erl_syntax:integer(Exp);
compile_exp(Exp, _Env) when is_float(Exp) -> % 2.2
  erl_syntax:float(Exp);

%% lambda
compile_exp([lambda, Var, Body], Env) when is_atom(Var) -> % (lambda X (+ X 2))
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, Var),
  Body2 = compile_exp(Body, Env2),
  Clause = erl_syntax:clause([erl_syntax:variable(VarName)], [], [Body2]),
  erl_syntax:fun_expr([Clause]);

%% let
compile_exp(['let', Var, Value, Body], Env) when is_atom(Var) -> % (let X (+ 2 2) (+ X 3))
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, Var),
  Value2 = compile_exp(Value, Env),
  Assignment = erl_syntax:match_expr(erl_syntax:variable(VarName), Value2),
  Body2 = compile_exp(Body, Env2),
  erl_syntax:block_expr([Assignment, Body2]);

%% Function application
compile_exp([Op | Args], Env) -> % (a b c)
  Args2 = [compile_exp(Arg, Env) || Arg <- Args],
  case shen_erl_kl_env:fetch(Env, Op) of
    {ok, VarName} ->
      erl_syntax:application(erl_syntax:variable(VarName), Args2);
    not_found ->
      case op_arity(Op) of
        {ok, Arity} -> compile_application(Op, Arity, Args2);
        not_found ->
          Op2 = compile_exp(Op, Env),
          erl_syntax:application(Op2, Args2)
      end
  end.

compile_application(Op, Arity, Args) when Arity =:= length(Args) ->
  erl_syntax:application(erl_syntax:atom(erlang), erl_syntax:atom(Op), Args).

op_arity('+') -> {ok, 2};
op_arity('*') -> {ok, 2};
op_arity('/') -> {ok, 2};
op_arity('-') -> {ok, 2};
op_arity('and') -> {ok, 2};
op_arity('or') -> {ok, 2};
op_arity(_) -> not_found.

%% Helper functions
fun_vars(Args, Env) ->
  {Args2, Env2} = lists:foldl(fun fun_var/2, {[], Env}, Args),
  {lists:reverse(Args2), Env2}.

fun_var(Arg, {Acc, Env}) when is_atom(Arg) ->
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, Arg),
  {[erl_syntax:variable(VarName) | Acc], Env2}.
