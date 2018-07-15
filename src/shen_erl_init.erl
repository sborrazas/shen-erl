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
  shen_erl_global_stores:init(),
  case init:get_plain_arguments() of
    [] ->
      io:format(standard_error, "shen-erl: No arguments provided.~n", []),
      init:stop(?ERROR_STATUS);
    ["--script", Filename | _Args] ->
      case shen_erl_kl_compiler:load(Filename) of
        ok ->
          io:format("File `~s` loaded successfully.~n", [Filename]),
          init:stop(?OK_STATUS);
        {error, Reason} ->
          io:format(standard_error, "Error ocurred: ~s~n", [Reason]),
          init:stop(?ERROR_STATUS)
      end;
    ["--eval", Code | _Args] ->
      case shen_erl_kl_compiler:eval(Code) of
        ok -> init:stop(?OK_STATUS);
        {error, Reason} ->
          io:format(standard_error, "Error ocurred: ~s~n", [Reason]),
          init:stop(?ERROR_STATUS)
      end;
    ["--kl" | Args] ->
      {Filenames, Opts} = parse_opts(Args),
      io:format(standard_error, "shen-erl: compiling ~p with opts ~p~n", [Filenames, Opts]),
      case shen_erl_kl_compiler:files_kl(Filenames, Opts) of
        ok ->
          io:format("Compiled successfully.~n", []),
          init:stop(?OK_STATUS);
        {error, Reason} ->
          io:format(standard_error, "Error ocurred: ~s~n", [Reason]),
          init:stop(?ERROR_STATUS)
      end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_opts(Args) ->
  parse_opts(Args, {[], []}).

parse_opts(["--output-dir", OutputDir | Rest], {Files, Opts}) ->
  parse_opts(Rest, {Files, [{output_dir, OutputDir} | Opts]});
parse_opts([Filename | Rest], {Files, Opts}) ->
  parse_opts(Rest, {[Filename | Files], Opts});
parse_opts([], {Files, Opts}) ->
  {lists:reverse(Files), Opts}.
