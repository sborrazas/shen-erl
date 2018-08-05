%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_codegen).

%% API
-export([load_defuns/2,
         eval/1,
         compile/2]).

%% Macros
-define(ERL_TRUE, erl_syntax:atom(true)).
-define(ERL_FALSE, erl_syntax:atom(false)).

%% Types
-type form() :: erl_syntax:syntaxTree().

-record(code, {signatures :: [form()],
               forms :: [form()],
               tles :: [form()]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec load_defuns(module(), shen_erl_kl_parse:kl_tree()) -> ok.
load_defuns(Mod, ToplevelDefs) ->
  [shen_erl_global_stores:set_mfa(Name, {Mod, Name, length(Args)}) ||
    [defun, Name, Args, _Body] <- ToplevelDefs, is_atom(Name), is_list(Args)].

-spec eval(shen_erl_kl_parse:kl_tree()) -> {ok, [{module(), binary()}]} |
                                           {error, binary()}.
eval(ToplevelDefs) ->
  eval_toplevel(ToplevelDefs, []).

-spec compile(module(), shen_erl_kl_parse:kl_tree()) -> {ok, binary()} |
                                                        {error, binary()}.
compile(Mod, ToplevelDefs) ->
  case compile_toplevel(ToplevelDefs, #code{signatures = [],
                                            forms = [],
                                            tles = [erl_syntax:atom(ok)]}) of
    {ok, #code{signatures = Signatures, forms = Forms, tles = Tles}} ->
      ModAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Mod)]),
      ModForm = erl_syntax:revert(ModAttr),

      TleSignature = erl_syntax:arity_qualifier(erl_syntax:atom(kl_tle), erl_syntax:integer(0)),

      ExportAttr = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list([TleSignature | Signatures])]),
      ExportForm = erl_syntax:revert(ExportAttr),

      TleClause =  erl_syntax:clause([], [], lists:reverse(Tles)), % No args, no guards
      TleFunction = erl_syntax:function(erl_syntax:atom(kl_tle), [TleClause]),
      TleForm = erl_syntax:revert(TleFunction),

      case compile:forms([ModForm, ExportForm, TleForm | Forms]) of
        {ok, Mod, Bin} -> {ok, Bin};
        SomethingElse -> {error, SomethingElse}
      end;
    {error, Reason} -> {error, Reason}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_toplevel([[defun, Name, Args, Body] | Rest],
                 Code = #code{signatures = Signatures,
                              forms = Forms}) when is_atom(Name) andalso is_list(Args) ->
  Env = shen_erl_kl_env:new(),
  {ArgsCode, Env2} = fun_vars(Args, Env),
  BodyCode = compile_exp(Body, Env2),
  Clause =  erl_syntax:clause(ArgsCode, [], [BodyCode]),
  Function = erl_syntax:function(erl_syntax:atom(Name), [Clause]),
  FunForm = erl_syntax:revert(Function),

  Signature = erl_syntax:arity_qualifier(erl_syntax:atom(Name), erl_syntax:integer(length(Args))),

  %% case Name of
  %%   'shen.<define>' -> io:format(standard_error, "PP: ~p~n", [erl_prettypr:format(FunForm)]);
  %%   _ -> ok
  %% end,

  compile_toplevel(Rest, Code#code{signatures = [Signature | Signatures],
                                   forms = [FunForm | Forms]});
compile_toplevel([Exp | Rest], Code = #code{tles = Tles}) ->
  Env = shen_erl_kl_env:new(),
  BodyCode = compile_exp(Exp, Env),
  compile_toplevel(Rest, Code#code{tles = [BodyCode | Tles]});
compile_toplevel([], Code) ->
  {ok, Code}.

eval_toplevel([[defun, Name, Args, Body] | Rest],
                 Modules) when is_atom(Name) andalso is_list(Args) ->
  SanitizedName = sanitize(Name),
  Env = shen_erl_kl_env:new(),
  {ArgsCode, Env2} = fun_vars(Args, Env),
  BodyCode = compile_exp(Body, Env2),
  Clause =  erl_syntax:clause(ArgsCode, [], [BodyCode]),
  Function = erl_syntax:function(erl_syntax:atom(SanitizedName), [Clause]),
  FunForm = erl_syntax:revert(Function),

  ModAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(SanitizedName)]),
  ModForm =  erl_syntax:revert(ModAttr),

  Signature = erl_syntax:arity_qualifier(erl_syntax:atom(SanitizedName), erl_syntax:integer(length(Args))),
  ExportAttr = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list([Signature])]),
  ExportForm = erl_syntax:revert(ExportAttr),

  %% case Name of
  %%   'shen.<define>' -> io:format(standard_error, "PP: ~p~n", [erl_prettypr:format(FunForm)]);
  %%   _ -> ok
  %% end,

  case compile:forms([ModForm, ExportForm, FunForm]) of
    {ok, Mod, Bin} -> eval_toplevel(Rest, [{Mod, Bin} | Modules]);
    SomethingElse -> {error, SomethingElse}
  end;
eval_toplevel([Exp | Rest], Modules) ->
  Name = list_to_atom("tle_" ++ integer_to_list(length(Modules))), % Top level expression function name
  io:format(standard_error, "COMPILING TLE (exp): ~p~n", [Name]),
  Env = shen_erl_kl_env:new(),

  BodyCode = compile_exp(Exp, Env),
  Clause =  erl_syntax:clause([], [], [BodyCode]), % No args, no guards
  Function = erl_syntax:function(erl_syntax:atom(Name), [Clause]),
  FunForm = erl_syntax:revert(Function),

  ModAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Name)]),
  ModForm =  erl_syntax:revert(ModAttr),

  Signature = erl_syntax:arity_qualifier(erl_syntax:atom(Name), erl_syntax:integer(0)), % No args
  ExportAttr = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list([Signature])]),
  ExportForm = erl_syntax:revert(ExportAttr),

  case compile:forms([ModForm, ExportForm, FunForm]) of
    {ok, Mod, Bin} -> eval_toplevel(Rest, [{Mod, Bin} | Modules]);
    SomethingElse -> {error, SomethingElse}
  end;
eval_toplevel([], Modules) ->
  {ok, Modules}.

%% Lists
compile_exp([], _Env) -> % ()
  erl_syntax:nil();

%% Boolean operators
compile_exp(['and', Exp1, Exp2], Env) -> % (and Exp1 Exp2)
  CExp1 = compile_exp(Exp1, Env),
  CExp2 = compile_exp(Exp2, Env),
  erl_syntax:infix_expr(CExp1, erl_syntax:operator("andalso"), CExp2);
compile_exp(['or', Exp1, Exp2], Env) -> % (or Exp1 Exp2)
  CExp1 = compile_exp(Exp1, Env),
  CExp2 = compile_exp(Exp2, Env),
  erl_syntax:infix_expr(CExp1, erl_syntax:operator("orelse"), CExp2);
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

%% Strings
compile_exp({string, Exp}, _Env) ->
  erl_syntax:tuple([erl_syntax:atom(string), erl_syntax:string(Exp)]);

%% lambda
compile_exp([lambda, Var, Body], Env) when is_atom(Var) -> % (lambda X (+ X 2))
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, Var),
  CBody = compile_exp(Body, Env2),
  Clause = erl_syntax:clause([erl_syntax:variable(VarName)], [], [CBody]),
  erl_syntax:fun_expr([Clause]);

%% let
compile_exp(['let', Var, ExpValue, Body], Env) when is_atom(Var) -> % (let X (+ 2 2) (+ X 3))
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, Var),
  CExpValue = compile_exp(ExpValue, Env),
  CBody = compile_exp(Body, Env2),
  Clause = erl_syntax:clause([erl_syntax:variable(VarName)], none, [CBody]),
  erl_syntax:case_expr(CExpValue, [Clause]);

%% if
compile_exp(['if', Exp, ExpTrue, ExpFalse], Env) ->
  CExp = compile_exp(Exp, Env),
  CExpTrue = compile_exp(ExpTrue, Env),
  CExpFalse = compile_exp(ExpFalse, Env),
  TrueClause = erl_syntax:clause([?ERL_TRUE], none, [CExpTrue]),
  FalseClause = erl_syntax:clause([?ERL_FALSE], none, [CExpFalse]),
  erl_syntax:case_expr(CExp, [TrueClause, FalseClause]);

%% cond
compile_exp(['cond', [Exp, ExpTrue] | Rest], Env) ->
  CExp = compile_exp(Exp, Env),
  CExpTrue = compile_exp(ExpTrue, Env),
  CExpFalse = compile_exp(['cond' | Rest], Env), % TODO: Check all the cond structure beforehand
  TrueClause = erl_syntax:clause([?ERL_TRUE], none, [CExpTrue]),
  FalseClause = erl_syntax:clause([?ERL_FALSE], none, [CExpFalse]),
  erl_syntax:case_expr(CExp, [TrueClause, FalseClause]);

compile_exp(['cond'], _Env) ->
  erl_syntax:application(erl_syntax:atom(throw),
                         [erl_syntax:tuple([erl_syntax:atom(kl_error),
                                            erl_syntax:string("End of cond reached")])]);

%% Lazy values
compile_exp([freeze, Body], Env) ->
  CBody = compile_exp(Body, Env),
  Clause = erl_syntax:clause([], [], [CBody]),
  erl_syntax:fun_expr([Clause]);

%% Trap errors
compile_exp(['trap-error', Body, Handler], Env) ->
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, newvar),
  {ErrorVarName, Env3} = shen_erl_kl_env:new_var(Env2, newvar),
  CBody = compile_exp(Body, Env3),
  CHandlerFun = compile_exp(Handler, Env3),
  CVar = erl_syntax:variable(VarName),
  CErrorVar = erl_syntax:variable(ErrorVarName),
  CClause = erl_syntax:clause([CVar], none, [CVar]),
  CError = erl_syntax:tuple([erl_syntax:atom(kl_error), CErrorVar]),
  CHandler = erl_syntax:clause([CError], none, [erl_syntax:application(CHandlerFun, [CErrorVar])]),
  erl_syntax:try_expr([CBody], [CClause], [CHandler]);

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
      case shen_erl_kl_primitives:fun_mfa(Op) of
        {ok, {Mod, FunName, Arity}} ->
          % 1.2.1: Function operator is a KL primitive
          compile_static_app(erl_syntax:module_qualifier(erl_syntax:atom(Mod), erl_syntax:atom(FunName)), Arity, CArgs, Env);
        not_found ->
          % 1.2.2: Function operator is a user-defined function
          case shen_erl_global_stores:get_mfa(Op) of
            {ok, {Mod, Fun, Arity}} ->
              CMod = erl_syntax:module_qualifier(erl_syntax:atom(Mod), erl_syntax:atom(Fun)),
              compile_static_app(CMod, Arity, CArgs, Env);
            not_found ->
              throw({invalid_fun, Op})
          end
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

compile_static_app(COp, Arity, CArgs, _Env) when Arity =:= length(CArgs) ->
  erl_syntax:application(COp, CArgs);
compile_static_app(COp, Arity, CArgs, Env) when Arity > length(CArgs) ->
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, newvar),
  CBody = compile_static_app(COp, Arity, CArgs ++ [erl_syntax:variable(VarName)], Env2),
  Clause = erl_syntax:clause([erl_syntax:variable(VarName)], [], [CBody]),
  erl_syntax:fun_expr([Clause]);
compile_static_app(COp, Arity, CArgs, Env) when Arity < length(CArgs) ->
  {StaticCArgs, DynamicCArgs} = lists:split(Arity, CArgs),
  compile_dynamic_app(compile_static_app(COp, Arity, StaticCArgs, Env), DynamicCArgs).

%% Helper functions
fun_vars(Args, Env) ->
  {Args2, Env2} = lists:foldl(fun fun_var/2, {[], Env}, Args),
  {lists:reverse(Args2), Env2}.

fun_var(Arg, {Acc, Env}) when is_atom(Arg) ->
  {VarName, Env2} = shen_erl_kl_env:new_var(Env, Arg),
  {[erl_syntax:variable(VarName) | Acc], Env2}.

sanitize(Name) ->
  list_to_atom("kl_" ++ re:replace(atom_to_list(Name), "[^\\w.-]", "__", [global, {return, list}])).
