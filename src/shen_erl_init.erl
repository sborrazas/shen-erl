%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_init).

%% API
-export([start/0]).

-define(OK_STATUS, 0).
-define(ERROR_STATUS, 1).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  case init:get_plain_arguments() of
    [] ->
      io:format(standard_error, "shen-erl: No arguments provided.~n", []),
      init:stop(?ERROR_STATUS);
    [Filename | Args] -> % TODO: Assuming kl file for now
      io:format(standard_error, "shen-erl: compiling ~p~n", [Filename]),
      case shen_erl_kl_compiler:file(Filename, Args) of
        {ok, Mod} ->
          io:format("~p module compiled successfully.~n", [Mod]),
          init:stop(?OK_STATUS);
        {error, Reason} ->
          io:format("Error ocurred: ~s~n", [Reason]),
          init:stop(?ERROR_STATUS)
      end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
