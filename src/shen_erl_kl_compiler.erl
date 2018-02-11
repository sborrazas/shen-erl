%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_compiler).

%% API
-export([file/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec file(string(), [string()]) -> {ok, atom()} | {error, binary()}.
file(Filename, _Args) ->
  case file:open(Filename, [read]) of
    {ok, In} ->
      case io:request(In, {get_until, unicode, '', shen_erl_kl_scan, tokens, [1]}) of
        {ok, Tokens, _EndLine} ->
          case shen_erl_kl_parse:parse_tree(Tokens) of
            {ok, ExpTree} ->
              case shen_erl_kl_codegen:compile(ExpTree) of
                {ok, Code} ->
                  io:format("CORE CODE: ~p~n", [Code]),
                  {ok, Filename};
                {error, Reason} -> {error, Reason}
              end;
            {error, Reason} ->
              io:format(standard_error, "ERROR: ~p~n", [Reason]),
              {error, Reason}
          end;
        {ErrorLine, Mod, Reason} ->
          io:format(standard_error, "ERROR: ~p, ~p, ~p~n", [ErrorLine, Mod, Reason]),
          {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
