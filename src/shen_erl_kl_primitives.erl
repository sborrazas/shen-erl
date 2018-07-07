%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_primitives).

%% API
-export([fun_mfa/1,
         'if'/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec fun_mfa(atom()) -> {ok, mfa()} | not_found.
fun_mfa('+') -> {ok, {erlang, '+', 2}};
fun_mfa('-') -> {ok, {erlang, '-', 2}};
fun_mfa('*') -> {ok, {erlang, '*', 2}};
fun_mfa('/') -> {ok, {erlang, '/', 2}};
fun_mfa('and') -> {ok, {erlang, 'and', 2}};
fun_mfa('or') -> {ok, {erlang, 'or', 2}};
fun_mfa('>') -> {ok, {erlang, '>', 2}};
fun_mfa('if') -> {ok, {?MODULE, 'if', 3}};
fun_mfa(_) -> not_found.

%% if
'if'(true, TrueVal, _FalseVal) -> TrueVal;
'if'(false, _TrueVal, FalseVal) -> FalseVal.

%%%===================================================================
%%% Internal functions
%%%===================================================================
