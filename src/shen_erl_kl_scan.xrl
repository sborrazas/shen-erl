Definitions.

S  = -?
D  = [0-9]+
WS = [\000-\s]
A  = [^()\"\000-\s]

Rules.

%% Numbers
{S}{D}+       : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{S}{D}+\.{D}+ : {token, {number, TokenLine, list_to_float(TokenChars)}}.
\.{D}+        : {token, {number, TokenLine, list_to_float([$0 | TokenChars])}}.

%% String
\"[^\"]*\"    : {token, {string, TokenLine, string:substr(TokenChars, 2, TokenLen - 2)}}.

%% Symbols
{A}+          : {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.

%% Parentheses
[()]          : {token, {list_to_atom(TokenChars), TokenLine}}.

%% Whitespace/comments
{WS}+         : skip_token.

Erlang code.
