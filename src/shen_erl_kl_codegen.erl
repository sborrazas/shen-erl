%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_codegen).

%% API
-export([compile/1]).

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

-spec compile(shen_erl_kl_parse:kl_tree()) -> {ok, [{module(), binary()}]} |
                                              {error, binary()}.
compile(ToplevelDefs) ->
  compile_toplevel(ToplevelDefs, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_toplevel([[defun, Name, Args, Body] | Rest], Acc) when is_atom(Name) ->
  Env = shen_erl_kl_env:new(),
  {ArgsCode, Env2} = fun_vars(Args, Env),
  BodyCode = compile_exp(Body, Env2),
  Clause =  erl_syntax:clause(ArgsCode, [], [BodyCode]),
  Function = erl_syntax:function(erl_syntax:atom(Name), [Clause]),
  FunForm = erl_syntax:revert(Function),

  Module = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Name)]),
  ModForm =  erl_syntax:revert(Module),

  Signature = erl_syntax:arity_qualifier(erl_syntax:atom(Name), erl_syntax:integer(length(Args))),
  Export = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list([Signature])]),
  ExportForm = erl_syntax:revert(Export),

  case compile:forms([ModForm, ExportForm, FunForm]) of
    {ok, Mod, Bin} -> compile_toplevel(Rest, [{Mod, Bin} | Acc]);
    SomethingElse -> {error, SomethingElse}
  end;
compile_toplevel([Exp | Rest], Acc) ->
  Name = tle, % Top level expression function name

  BodyCode = compile_exp(Exp, shen_erl_kl_env:new()),
  Clause =  erl_syntax:clause([], [], [BodyCode]), % No args, no guards
  Function = erl_syntax:function(erl_syntax:atom(Name), [Clause]),
  FunForm = erl_syntax:revert(Function),

  Module = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Name)]),
  ModForm =  erl_syntax:revert(Module),

  Signature = erl_syntax:arity_qualifier(erl_syntax:atom(Name), erl_syntax:integer(0)), % No args
  Export = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list([Signature])]),
  ExportForm = erl_syntax:revert(Export),

  case compile:forms([ModForm, ExportForm, FunForm]) of
    {ok, Mod, Bin} -> compile_toplevel(Rest, [{Mod, Bin} | Acc]);
    SomethingElse -> {error, SomethingElse}
  end;
compile_toplevel([], Acc) ->
  {ok, Acc}.

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
  CBody = compile_exp(Body, Env2),
  Clause = erl_syntax:clause([erl_syntax:variable(VarName)], [], [CBody]),
  erl_syntax:fun_expr([Clause]);

%% let
compile_exp(['let', Var, Value, Body], Env) when is_atom(Var) -> % (let X (+ 2 2) (+ X 3))
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, Var),
  Value2 = compile_exp(Value, Env),
  Assignment = erl_syntax:match_expr(erl_syntax:variable(VarName), Value2),
  CBody = compile_exp(Body, Env2),
  erl_syntax:block_expr([Assignment, CBody]);

%% if
compile_exp(['if', Exp, ExpTrue, ExpFalse], Env) ->
  CExp = compile_exp(Exp, Env),
  CExpTrue = compile_exp(ExpTrue, Env),
  CExpFalse = compile_exp(ExpFalse, Env),
  TrueClause = erl_syntax:clause([erl_syntax:atom(true)], none, [CExpTrue]),
  FalseClause = erl_syntax:clause([erl_syntax:atom(false)], none, [CExpFalse]),
  erl_syntax:case_expr(CExp, [TrueClause, FalseClause]);

%% Lazy values
compile_exp([freeze, Body], Env) ->
  CBody = compile_exp(Body, Env),
  Clause = erl_syntax:clause([], [], [CBody]),
  erl_syntax:fun_expr([Clause]);

%% Function applications

%% Case 1: Function operator is an atom
compile_exp([Op | Args], Env) when is_atom(Op) -> % (a b c)
  CArgs = [compile_exp(Arg, Env) || Arg <- Args],
  case shen_erl_kl_env:fetch(Env, Op) of
    {ok, VarName} ->
      %% Case 1.1: Function operator is a variable
      compile_dynamic_app(erl_syntax:variable(VarName), CArgs);
    not_found ->
      %% Case 1.2: Function operator is a global function
      case op_arity(Op) of
        {ok, Arity} ->
          % 1.2.1: Function operator is a global predefined function
          compile_static_app(Op, Arity, CArgs, Env);
        not_found ->
          % 1.2.2: Function operator is a global user-defined function
          erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(Op), erl_syntax:atom(Op)), CArgs)
      end
  end;

%% Case 2: Function operator is not an atom
compile_exp([Op | Args], Env) ->
  COp = compile_exp(Op, Env),
  CArgs = [compile_exp(Arg, Env) || Arg <- Args],
  compile_dynamic_app(COp, CArgs).

compile_dynamic_app(COp, []) -> % Freezed expression application
  erl_syntax:application(COp, []);
compile_dynamic_app(COp, [CArg]) -> % Single argument application
  erl_syntax:application(COp, [CArg]);
compile_dynamic_app(COp, [CArg | RestCArgs]) -> % Multiple argument application
  compile_dynamic_app(compile_dynamic_app(COp, [CArg]), RestCArgs).

compile_static_app(Op, Arity, Args, _Env) when Arity =:= length(Args) ->
  erl_syntax:application(erl_syntax:atom(erlang), erl_syntax:atom(Op), Args);
compile_static_app(Op, Arity, Args, Env) when Arity > length(Args) ->
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, newvar),
  CBody = compile_static_app(Op, Arity, Args ++ [erl_syntax:variable(VarName)], Env2),
  Clause = erl_syntax:clause([erl_syntax:variable(VarName)], [], [CBody]),
  erl_syntax:fun_expr([Clause]).

op_arity('+') -> {ok, 2};
op_arity('*') -> {ok, 2};
op_arity('/') -> {ok, 2};
op_arity('-') -> {ok, 2};
op_arity('and') -> {ok, 2};
op_arity('or') -> {ok, 2};
op_arity('>') -> {ok, 2};
op_arity(_) -> not_found.

%% Helper functions
fun_vars(Args, Env) ->
  {Args2, Env2} = lists:foldl(fun fun_var/2, {[], Env}, Args),
  {lists:reverse(Args2), Env2}.

fun_var(Arg, {Acc, Env}) when is_atom(Arg) ->
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, Arg),
  {[erl_syntax:variable(VarName) | Acc], Env2}.
