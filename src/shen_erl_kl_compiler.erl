%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_compiler).

%% API
-export([start_repl/0,
         files_kl/2,
         eval_kl/1,
         eval/1,
         load/1]).

%% Macros
-define(KL_MODS, ['kl_core',
                  'kl_dict',
                  'kl_load',
                  'kl_prolog',
                  'kl_sequent',
                  'kl_t-star',
                  'kl_track',
                  'kl_writer',
                  'kl_declarations',
                  'kl_macros',
                  'kl_reader',
                  'kl_sys',
                  'kl_toplevel',
                  'kl_types',
                  'kl_yacc',
                  'kl_init',
                  'kl_extension-features',
                  'kl_extension-launcher',
                  'kl_extension-factorise-defun']).

%% Types
-type opt() :: {output_dir, string()}.

-export_type([opt/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec files_kl([string()], [opt()]) -> ok | {error, binary()}.
files_kl(Filenames, Opts) ->
  case parse_files(Filenames, []) of
    {ok, FilesAsts} -> compile_kl(FilesAsts, Opts);
    {error, Reason} -> {error, Reason}
  end.

-spec eval_kl(term()) -> term().
eval_kl(KlCode) ->
  shen_erl_kl_codegen:eval(KlCode).

-spec load(string()) -> ok.
load(Filename) ->
  load_funs(),
  kl_load:load({string, Filename}),
  ok.

-spec eval(string()) -> term().
eval(ShenCode) ->
  load_funs(),
  kl_sys:eval(shen_erl_kl_primitives:'hd'(kl_reader:'read-from-string'({string, ShenCode}))).

-spec start_repl() -> ok.
start_repl() ->
  load_funs(),
  kl_toplevel:'shen.repl'().

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_funs() ->
  [[shen_erl_global_stores:set_mfa(FunName, {Mod, FunName, Arity}) ||
     {FunName, Arity} <- Mod:module_info(exports),
     FunName =/= kl_tle, FunName =/= module_info] || Mod <- ?KL_MODS],
  kl_init:'shen.initialise'().

compile_kl([{Mod, Ast} | Rest], Opts) ->
  io:format(standard_error, "COMPILING ~p~n", [Mod]),
  case shen_erl_kl_codegen:compile(Mod, Ast, ok) of
    {ok, Mod, Bin} ->
      case write(Mod, Bin, Opts) of
        ok -> compile_kl(Rest, Opts);
        {error, Reason} -> {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end;
compile_kl([], _Opts) ->
  ok.

parse_files([Filename | Rest], Acc) ->
  case parse_kl_file(Filename) of
    {ok, Ast} ->
      Mod = list_to_atom("kl_" ++ filename:basename(Filename, ".kl")),
      shen_erl_kl_codegen:load_defuns(Mod, Ast),
      parse_files(Rest, [{Mod, Ast} | Acc]);
    {error, Reason} -> {error, Reason}
  end;
parse_files([], Acc) -> {ok, Acc}.

parse_kl_file(Filename) ->
  case file:open(Filename, [read]) of
    {ok, In} ->
      case io:request(In, {get_until, unicode, '', shen_erl_kl_scan, tokens, [1]}) of
        {ok, Tokens, _EndLine} ->
          shen_erl_kl_parse:parse_tree(Tokens);
        {error, Reason} -> {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.

write(Mod, BeamCode, Opts) ->
  {ok, CurrentDir} = file:get_cwd(),
  OutputDir = proplists:get_value(output_dir, Opts, CurrentDir),
  case file:write_file(OutputDir ++ "/" ++ atom_to_list(Mod) ++ ".beam", BeamCode) of
    ok -> ok;
    {error, Reason} -> {error, Reason}
  end.
