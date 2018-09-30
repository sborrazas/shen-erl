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
  erlang:apply(Mod, Fun, Args).

'erl.receive'(Timeout) ->
  receive
    Message -> Message
  after
    Timeout -> timeout
  end.

'erl.send'(Pid, Message) ->
  Pid ! Message.
