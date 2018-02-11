%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_codegen).

%% API
-export([compile/1]).

%% Types
-type core_lit() :: {var, string()} |
                    {atom, atom()}.

-type core_exp() :: core_lit() |
                    {list, [core_lit()]}.

-type form() :: [core_exp()].

-record(code, {forms :: [{atom(), form()}],
               toplevel :: form()}).

-opaque code() :: #code{}.

-export_type([code/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec compile(term()) -> code().
compile(ToplevelExps) ->
  compile_toplevel(ToplevelExps, shen_erl_kl_env:new(), #code{forms = #{},
                                                              toplevel = []}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_toplevel([ToplevelExp | Rest], Env, Code) ->
  case kl_to_core(ToplevelExp, Env, Code) of
    {ok, Env2, Code2} -> compile_toplevel(Rest, Env2, Code2);
    {error, Reason} -> {error, Reason}
  end;
compile_toplevel([], _Env, Code = #code{toplevel = Toplevel}) ->
  {ok, Code#code{toplevel = lists:reverse(Toplevel)}}.

kl_to_core([defun, Name, _Args, Body], Env, Code = #code{forms = Forms}) ->
  case compile_exp(Body, shen_erl_kl_env:new()) of
    {ok, FunCode} -> {ok, Env, Code#code{forms = Forms#{Name => FunCode}}};
    {error, Reason} -> {error, Reason}
  end;
kl_to_core(Exp, Env, Code = #code{toplevel = Toplevel}) ->
  case compile_exp(Exp, Env) of
    {ok, ToplevelCode} -> {ok, Code#code{toplevel = Toplevel ++ ToplevelCode}};
    {error, Reason} -> {error, Reason}
  end.

compile_exp([], _Env) ->
  {ok, {list, []}};
compile_exp(Exp, Env) when is_atom(Exp) ->
  case shen_erl_kl_env:fetch(Env, Exp) of
    {ok, VarName} -> {ok, {var, VarName}};
    not_found -> {ok, {atom, Exp}}
  end;
compile_exp(_Exp, Env) -> % Literal
  {ok, {atom, pending}}. % TODO
