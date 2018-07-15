%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_primitives).

%% API
-export([fun_mfa/1,
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
         'cn'/1,
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

-spec fun_mfa(atom()) -> {ok, mfa()} | not_found.
fun_mfa('+') -> {ok, {erlang, '+', 2}};
fun_mfa('-') -> {ok, {erlang, '-', 2}};
fun_mfa('*') -> {ok, {erlang, '*', 2}};
fun_mfa('/') -> {ok, {erlang, '/', 2}};
fun_mfa('and') -> {ok, {erlang, 'and', 2}};
fun_mfa('or') -> {ok, {erlang, 'or', 2}};
fun_mfa('>') -> {ok, {erlang, '>', 2}};
fun_mfa('<') -> {ok, {erlang, '<', 2}};
fun_mfa('>=') -> {ok, {erlang, '>=', 2}};
fun_mfa('<=') -> {ok, {erlang, '<=', 2}};
fun_mfa('if') -> {ok, {?MODULE, 'if', 3}};
fun_mfa('simple-error') -> {ok, {?MODULE, 'simple-error', 1}};
fun_mfa('error-to-string') -> {ok, {?MODULE, 'error-to-string', 1}};
fun_mfa('intern') -> {ok, {?MODULE, 'intern', 1}};
fun_mfa('set') -> {ok, {?MODULE, 'set', 2}};
fun_mfa('value') -> {ok, {?MODULE, 'value', 1}};
fun_mfa('number?') -> {ok, {?MODULE, 'number?', 1}};
fun_mfa('string?') -> {ok, {?MODULE, 'string?', 1}};
fun_mfa('pos') -> {ok, {?MODULE, 'pos', 2}};
fun_mfa('tlstr') -> {ok, {?MODULE, 'tlstr', 1}};
fun_mfa('str') -> {ok, {?MODULE, 'str', 1}};
fun_mfa('cn') -> {ok, {?MODULE, 'cn', 1}};
fun_mfa('string->n') -> {ok, {?MODULE, 'string->n', 1}};
fun_mfa('n->string') -> {ok, {?MODULE, 'n->string', 1}};
fun_mfa('absvector') -> {ok, {?MODULE, 'absvector', 1}};
fun_mfa('address->') -> {ok, {?MODULE, 'address->', 3}};
fun_mfa('<-address') -> {ok, {?MODULE, '<-address', 2}};
fun_mfa('absvector?') -> {ok, {?MODULE, 'absvector?', 1}};
fun_mfa('cons?') -> {ok, {?MODULE, 'cons?', 1}};
fun_mfa('cons') -> {ok, {?MODULE, 'cons', 2}};
fun_mfa('hd') -> {ok, {?MODULE, 'hd', 1}};
fun_mfa('tl') -> {ok, {?MODULE, 'tl', 1}};
fun_mfa('write-byte') -> {ok, {?MODULE, 'write-byte', 2}};
fun_mfa('read-byte') -> {ok, {?MODULE, 'read-byte', 1}};
fun_mfa('open') -> {ok, {?MODULE, 'open', 2}};
fun_mfa('close') -> {ok, {?MODULE, 'close', 1}};
fun_mfa('=') -> {ok, {?MODULE, '=', 2}};
fun_mfa('eval-kl') -> {ok, {?MODULE, 'eval-kl', 1}};
fun_mfa('get-time') -> {ok, {?MODULE, 'get-time', 1}};
fun_mfa('type') -> {ok, {?MODULE, 'type', 2}};
fun_mfa(_) -> not_found.

%% if
'if'(true, TrueVal, _FalseVal) -> TrueVal;
'if'(false, _TrueVal, FalseVal) -> FalseVal.

%% simple-error
'simple-error'(ErrorMsg) when is_list(ErrorMsg) ->
  throw({'simple-error', ErrorMsg}).

%% error-to-string
'error-to-string'(Error) ->
  throw({'error-to-string', Error}).

%% intern
intern(SymbolStr) when is_list(SymbolStr) -> list_to_atom(SymbolStr).

%% set
set(Name, Val) when is_atom(Name) ->
  throw({'set', Name, Val}).

%% value
value(Name) when is_atom(Name) ->
  throw({value, Name}).

%% number?
'number?'(Val) when is_number(Val) -> true;
'number?'(_Val) -> false.

%% string?
'string?'(Val) when is_list(Val) -> true; % TODO: Determine string
'string?'(_Val) -> false.

%% pos
pos(Str, Index) -> string:substr(Str, Index, 1).

%% tlstr
tlstr(Str) -> string:substr(Str, 1).

%% str
str(Val) -> throw({str, Val}).

%% cn
cn(Val) ->
  throw({cn, Val}).

%% string->n
'string->n'([Char | _RestStr]) -> Char.

%% n->string
'n->string'(Char) -> [Char].

%% absvector
absvector(Length) ->
  throw({absvector, Length}).

%% address->
'address->'(Vec, Index, Val) ->
  throw({'address->', Vec, Index, Val}).

%% <-address
'<-address'(Vec, Index) ->
  throw({'<-address', Vec, Index}).

%% absvector?
'absvector?'(Val) ->
  throw({'absvector?', Val}).

%% cons?
'cons?'([_H | _T]) -> true;
'cons?'(_Val) -> false.

%% cons
cons(H, T) -> [H | T].

%% hd
hd([H | _T]) -> H.

%% tl
tl([_H | T]) -> T.

%% write-byte
'write-byte'(Num, Stream) ->
  throw({'write-byte', Num, Stream}).

%% read-byte
'read-byte'(Stream) ->
  throw({'read-byte', Stream}).

%% open
open(FilePath, in) -> throw({open, FilePath, in});
open(FilePath, out) -> throw({open, FilePath, out}).

%% close
close(Stream) ->
  throw({close, Stream}).

%% =
'='(Val1, Val2) ->
  throw({'=', Val1, Val2}).

%% eval-kl
'eval-kl'(Kl) -> throw({'eval-kl', Kl}).

%% get-time
'get-time'(unix) -> throw({'get-time', unix});
'get-time'(run) -> throw({'get-time', run}).

%% type
type(Val, _Hint) -> Val.

%%%===================================================================
%%% Internal functions
%%%===================================================================
