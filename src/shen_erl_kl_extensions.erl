%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_extensions).

%% API
-export(['erl.apply'/3,
         'erl.receive'/1,
         'erl.send'/2]).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

flatten_kl({cons, Car, Cdr}) ->
  [flatten_kl(Car) | flatten_kl(Cdr)];
flatten_kl({string, Str}) ->
  Str;
flatten_kl(Code) ->
  Code.
