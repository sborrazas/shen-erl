%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_extensions).

%% API
-export(['erl.apply'/3,
         'erl.receive'/1,
         'erl.send'/2,
         'assert-boolean'/1]).

%%%===================================================================
%%% API
%%%===================================================================

'erl.apply'(Mod, Fun, Args) ->
  erlang:apply(Mod, Fun, flatten_kl(Args)).

'erl.receive'(Timeout) ->
  receive
    Message -> Message
  after
    Timeout -> timeout
  end.

'erl.send'(Pid, Message) ->
  Pid ! Message.

'assert-boolean'(true) -> true;
'assert-boolean'(false) -> false;
'assert-boolean'(Value) ->
  ErrorMsg = io_lib:format("Expected a boolean in if/and/or/cond expression, got `~p`", [Value]),
  shen_erl_kl_primitives:'simple-error'({string, ErrorMsg}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

flatten_kl({cons, Car, Cdr}) ->
  [flatten_kl(Car) | flatten_kl(Cdr)];
flatten_kl({string, Str}) ->
  Str;
flatten_kl(Code) ->
  Code.
