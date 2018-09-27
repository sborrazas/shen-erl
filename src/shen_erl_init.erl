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
  init(),
  case init:get_plain_arguments() of
    [] ->
      %% shen_erl_kl_compiler:start_repl(),
      init:stop(?OK_STATUS);
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

init() ->
  shen_erl_global_stores:init(),
  shen_erl_kl_primitives:set('*stoutput*', standard_io),
  shen_erl_kl_primitives:set('*stinput*', standard_io),
  {ok, Cwd} = file:get_cwd(),
  shen_erl_kl_primitives:set('*home-directory*', {string, Cwd}),
  shen_erl_kl_primitives:set('*language*', {string, "Erlang"}),
  shen_erl_kl_primitives:set('*implementation*', {string, "Erlang OTP " ++ erlang:system_info(otp_release)}),
  shen_erl_kl_primitives:set('*os*', {string, "BEAM " ++ erlang:system_info(otp_release)}),
  case proplists:lookup(shen_erl, application:which_applications()) of
    {shen_erl, _Desc, Version} -> shen_erl_kl_primitives:set('*release*', {string, Version});
    none -> shen_erl_kl_primitives:set('*port*', {string, "Undefined"})
  end,
  shen_erl_kl_primitives:set('*porters*', {string, "Sebastian Borrazas"}).

parse_opts(Args) ->
  parse_opts(Args, {[], []}).

parse_opts(["--output-dir", OutputDir | Rest], {Files, Opts}) ->
  parse_opts(Rest, {Files, [{output_dir, OutputDir} | Opts]});
parse_opts([Filename | Rest], {Files, Opts}) ->
  parse_opts(Rest, {[Filename | Files], Opts});
parse_opts([], {Files, Opts}) ->
  {lists:reverse(Files), Opts}.
