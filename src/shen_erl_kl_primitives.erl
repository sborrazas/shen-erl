%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_primitives).

%% API
-export(['+'/2,
         '-'/2,
         '*'/2,
         '/'/2,
         'and'/2,
         'or'/2,
         '>'/2,
         '<'/2,
         '>='/2,
         '<='/2,
         'if'/3,
         'simple-error'/1,
         'error-to-string'/1,
         'intern'/1,
         'set'/2,
         'value'/1,
         'number?'/1,
         'string?'/1,
         'pos'/2,
         'tlstr'/1,
         'str'/1,
         'cn'/2,
         'string->n'/1,
         'n->string'/1,
         'absvector'/1,
         'address->'/3,
         '<-address'/2,
         'absvector?'/1,
         'cons?'/1,
         'cons'/2,
         'hd'/1,
         'tl'/1,
         'write-byte'/2,
         'read-byte'/1,
         'open'/2,
         'close'/1,
         '='/2,
         'eval-kl'/1,
         'get-time'/1,
         'type'/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% +
'+'(A, B) -> A + B.

%% -
'-'(A, B) -> A - B.

%% *
'*'(A, B) -> A * B.

%% /
'/'(A, B) -> A / B.

%% and
'and'(A, B) -> A and B.

%% or
'or'(A, B) -> A or B.

%% >
'>'(A, B) -> A > B.

%% <
'<'(A, B) -> A < B.

%% >=
'>='(A, B) -> A >= B.

%% <=
'<='(A, B) -> A =< B.

%% if
'if'(true, TrueVal, _FalseVal) -> TrueVal;
'if'(false, _TrueVal, FalseVal) -> FalseVal.

%% simple-error
'simple-error'({string, ErrorMsg}) -> throw({simple_error, ErrorMsg}).

%% error-to-string
'error-to-string'({simple_error, ErrorMsg}) ->
  {string, ErrorMsg};
'error-to-string'({Class, Body}) ->
  {string, lists:flatten(io_lib:format("~p:~p", [Class, Body]))}.

%% intern
intern({string, SymbolStr}) -> list_to_atom(SymbolStr).

%% set
set(Name, Val) when is_atom(Name) ->
  shen_erl_global_stores:set_val(Name, Val),
  Val.

%% value
value(Key) when is_atom(Key) ->
  case shen_erl_global_stores:get_val(Key) of
    {ok, Val} -> Val;
    not_found -> 'simple-error'({string, io_lib:format("Value not found for key `~p`", [Key])})
  end.

%% number?
'number?'(Val) when is_number(Val) -> true;
'number?'(_Val) -> false.

%% string?
'string?'({string, _Str}) -> true;
'string?'(_Val) -> false.

%% pos
pos({string, Str}, Index) when Index =< length(Str) ->
  {string, string:substr(Str, Index + 1, 1)};
pos({string, _Str}, Index) ->
  'simple-error'({string, io_lib:format("Index `~B` out of bounds.", [Index])}).

%% tlstr
tlstr({string, [_H | T]}) -> {string, T};
tlstr({string, []}) -> 'simple-error'({string, "Cannot call tlstr on an empty string."}).

%% str
str(Val) when is_atom(Val) ->
  {string, atom_to_list(Val)};
str({string, Str}) ->
  {string, lists:flatten(io_lib:format("~p", [Str]))};
str(Val) when is_function(Val) ->
  {string, lists:flatten(io_lib:format("FUN: ~p", [Val]))};
str(Val) when is_number(Val) ->
  {string, lists:flatten(io_lib:format("~p", [Val]))};
str(Val) when is_pid(Val) ->
  {string, lists:flatten(io_lib:format("PID: ~p", [Val]))};
str({cons, Car, Cdr}) ->
  {string, StrCar} = str(Car),
  {string, StrCdr} = str(Cdr),
  {string, lists:flatten(io_lib:format("(~s, ~s)", [StrCar, StrCdr]))};
str({vector, Vec}) ->
  {string, lists:flatten(io_lib:format("VECTOR: ~p", [Vec]))};
str([]) ->
  {string, "[]"}.

%% cn
cn({string, Str1}, {string, Str2}) -> {string, Str1 ++ Str2}.

%% string->n
'string->n'({string, [Char | _RestStr]}) -> Char;
'string->n'({string, []}) ->
  'simple-error'({string, "Cannot call string->n on an empty string."}).

%% n->string
'n->string'(Char) -> {string, [Char]}.

%% absvector
absvector(Length) ->
  {vector, shen_erl_kl_vector:new(Length)}.

%% address->
'address->'({vector, Vec}, Index, Val) ->
  shen_erl_kl_vector:set(Vec, Index, Val),
  {vector, Vec}.

%% <-address
'<-address'({vector, Vec}, Index) ->
  case shen_erl_kl_vector:get(Vec, Index) of
    {ok, Val} -> Val;
    out_of_bounds -> 'simple-error'({string, "Index out of bounds"})
  end.

%% absvector?
'absvector?'({vector, _Vec}) -> true;
'absvector?'(_Val) -> false.

%% cons?
'cons?'({cons, _H, _T}) -> true;
'cons?'(_Val) -> false.

%% cons
cons(H, T) -> {cons, H, T}.

%% hd
hd({cons, H, _T}) -> H;
hd(_Val) -> 'simple-error'({string, "Not a cons"}).

%% tl
tl({cons, _H, T}) -> T;
tl(_Val) -> 'simple-error'({string, "Not a cons"}).

%% write-byte
'write-byte'(Num, Stream) ->
  io:fwrite(Stream, "~c", [Num]).

%% read-byte
'read-byte'(Stream) ->
  case io:get_chars(Stream, [], 1) of
    [Char] -> Char;
    eof -> -1
  end.

%% open
open({string, FilePath}, in) ->
  {string, HomePath} = value('*home-directory*'),
  FileAbsPath = filename:absname(FilePath, HomePath),
  {ok, File} = file:open(FileAbsPath, [read]),
  File;
open({string, FilePath}, out) ->
  {string, HomePath} = value('*home-directory*'),
  FileAbsPath = filename:absname(FilePath, HomePath),
  {ok, File} = file:open(FileAbsPath, [write]),
  File.

%% close
close(Stream) ->
  file:close(Stream),
  [].

%% =
'='(Val1, Val2) ->
  Val1 =:= Val2.

%% eval-kl
'eval-kl'(KlCode) ->
  KlCodeFlat = flatten_kl_code(KlCode),
  shen_erl_kl_compiler:eval_kl(KlCodeFlat).

%% get-time
'get-time'(unix) ->
  Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - Epoch;
'get-time'(run) ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - shen_erl_global_stores:start_time().

%% type
type(Val, _Hint) -> Val.

%%%===================================================================
%%% Internal functions
%%%===================================================================

flatten_kl_code({cons, Car, Cdr}) ->
  [flatten_kl_code(Car) | flatten_kl_code(Cdr)];
flatten_kl_code(Code) ->
  Code.
