%%%-------------------------------------------------------------------
%%% @author Sebastian Borrazas
%%% @copyright (C) 2018, Sebastian Borrazas
%%%-------------------------------------------------------------------
-module(shen_erl_kl_vector).

-behaviour(gen_server).

%% API
-export([new/1,
         set/3,
         get/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-define(SERVER, ?MODULE).

%% Types
-type length() :: non_neg_integer().
-type index() :: non_neg_integer().

-opaque vec() :: pid().

-export_type([length/0,
              index/0,
              vec/0]).

-record(state, {length :: length(),
                array :: array:array()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(length()) -> {ok, vec()}.
new(Length) ->
  gen_server:start_link(?MODULE, [Length], []).

-spec set(vec(), index(), term()) -> ok.
set(Vec, Index, Term) ->
  gen_server:cast(Vec, {set, Index, Term}).

-spec get(vec(), index()) -> term().
get(Vec, Index) ->
  gen_server:call(Vec, {get, Index}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Length]) ->
  process_flag(trap_exit, true),
  {ok, #state{length = Length,
              array = array:new([{size, Length}, fixed])}}.

handle_call({get, Index}, _From, State = #state{length = Length,
                                                array = Array}) when Index < Length ->
  Reply = {ok, array:get(Index, Array)},
  {reply, Reply, State};
handle_call({get, _Index}, _From, State) ->
  {reply, out_of_bounds, State}.

handle_cast({set, Index, Value}, State = #state{array = Array}) ->
  {noreply, State#state{array = array:set(Index, Value, Array)}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, Status) ->
  Status.
