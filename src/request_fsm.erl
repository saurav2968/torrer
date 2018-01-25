%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 12:15 AM
%%%-------------------------------------------------------------------
-module(request_fsm).
-author("saurav").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_validate/3,
  state_wait/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

-define(SERVER, ?MODULE).
-include("../include/torrer.hrl").
-record(state, {filename, infohash, torrentdict}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
  Name = list_to_atom(atom_to_list(?SERVER) ++ Args),
  gen_statem:start_link({local, Name}, ?MODULE, Args, []).

callback_mode() -> state_functions.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init(FileName) ->
  lager:info("Starting request_fsm for ~p~n", [FileName]),
  gproc:reg({n, l, "request:" ++ FileName}),
  {ok, state_validate, #state{filename=FileName},[{next_event, internal, validate}]}.

format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

state_validate(internal, validate, #state{filename = FileName} = State) ->
  case file:read_file(FileName) of
    {ok, BinaryContents} ->
      case catch bencoding:decode(BinaryContents) of
        {ok, {DecodedContents, _}} ->
          lager:info("Decoded torrent file."),
          %% get SHA1 HASh of info key and store in state
          InfoHash = binary_to_list(torrer_utils:get_infohash(bencoding:encode(maps:get(<<"info">>, DecodedContents)))),
          lager:info("InfoHash ~p", [InfoHash]),
          {next_state, state_validate, State#state{infohash = InfoHash, torrentdict = DecodedContents}, [{next_event, internal, check_downloaded}]};
        {'EXIT', Reason} ->
          lager:error("Unable to decode torrent file ~p: ~p", [FileName, Reason]),
          {stop, {shutdown, Reason}}
      end;
    {error, Reason} ->
      lager:error("Error while reading torrent file ~p: ~p~n", [FileName, Reason]),
      {stop, {shutdown, Reason}}
  end;

state_validate(internal, check_downloaded, #state{filename = FileName, infohash = InfoHash, torrentdict = TorrentDict} = State) ->
  %% ask mnesia torrents table if this is already dwnloaded by checking if there is entry and field is true
  case db_controller:update_with_repeat(fun db_controller:check_downloaded/1, [InfoHash]) of
    {ok, true} ->
      lager:info("File ~p already downloaded", [FileName]),
      {stop, {shutdown, already_downloaded}};
    {error, Reason} -> {stop, {shutdown, Reason}};
    {ok, false} ->
      %% start child of torrent_sup
      case torrent_sup:new_torrent(InfoHash, TorrentDict) of
        {error, R} -> {stop, {shutdown, R}};
        _ ->
          %%%% meta table has decoded torrent dict
          lager:info("Adding entry into meta table for InfoHash: ~ts", [InfoHash]),
          ok = db_controller:update_with_repeat(fun db_controller:add_meta/2, [InfoHash, TorrentDict]),
          Torrent = torrer_utils:get_torrent_record(InfoHash, TorrentDict),
          lager:info("TorrentRECORD is ~p",[{Torrent#torrents.downloaded, Torrent#torrents.uploaded, Torrent#torrents.left}]),
          ok = db_controller:update_with_repeat(fun db_controller:add_torrent/1, [Torrent]),
          {next_state, state_wait, State}
      end
  end.

state_wait(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

terminate(Reason, _StateName, #state{filename = FileName} = _State) ->
  lager:info("Terminating request for ~p with Reason~p~nUnregistering from gproc", [FileName, Reason]),
  catch gproc:unreg({n, l, "request:" ++ FileName}),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
