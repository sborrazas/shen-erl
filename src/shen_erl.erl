-module(shen_erl).

-export([start/0]).

-define(OK_STATUS, 0).

start() ->
  io:format(standard_error, "shen-erl!", []),
  init:stop(?OK_STATUS).
