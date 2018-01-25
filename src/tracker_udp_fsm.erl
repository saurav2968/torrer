%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2018 9:23 PM
%%%-------------------------------------------------------------------
-module(tracker_udp_fsm).
-author("saurav").

-behaviour(gen_statem).

-include("../include/torrer.hrl").
%% API
-export([start_link/3]).

%% gen_statem callbacks
-export([
  init/1,
  callback_mode/0,
  format_status/2,
  state_connect/3,
  state_connecting/3,
  state_announce/3,
  state_announcing/3,
  state_bootstrap/3,
  state_name/3,
  state_stop/3,
  handle_event/4,
  terminate/3,
  code_change/4
]).

-define(SERVER, ?MODULE).
-define(PROTOCOL_ID, 4497486125440). %% hex to decimal
-define(CONNECT_SEQ, 0).
-define(ANNOUNCE_SEQ, 1).
-define(CONNECTION_ID_LIFETIME, 50000). % 50 secs

-record(state, {host, port, infohash, torrentdict, socket,
  peerid, event, transaction_id, action, connection_id, connection_time}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(InfoHash, TorrentDict, Uri) ->
  Name = torrer_utils:get_uuid(),
  gen_statem:start_link({local, Name}, ?MODULE, [InfoHash, TorrentDict, Uri], []).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
callback_mode() -> state_functions.

init([InfoHash, TorrentDict, Uri]) ->
  case torrer_utils:get_host_and_port_from_uri(Uri) of
    {error, Reason} ->
      {ok, state_stop, #state{},[{next_event, internal, Reason}]};
    {Host, Port} ->
      lager:info("Starting tracker_udp_fsm for InfoHash: ~p, Host: ~p",[InfoHash, {Host,Port}]),
      gproc:reg({n, l,{?SERVER, InfoHash, {Host, Port}}}),
      {ok, state_bootstrap, #state{infohash = InfoHash, host = Host, port = Port, torrentdict = TorrentDict},
        [{next_event, internal, ok}]}
    end.

format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

state_bootstrap(internal, ok, #state{host = Host} = State) ->
  %% create a udp socket
  {ok, PeerIdRecord} = db_controller:update_with_repeat(fun db_controller:get_meta/1, [peer_id]),
  {ok, Socket} = gen_udp:open(0, [{active, true}, binary]),
  lager:info("Opened socket for ~p: ~p", [ Host, Socket]),
  {next_state, state_connect, State#state{peerid = PeerIdRecord#meta.value, socket = Socket}, [{next_event, internal, connect}]}.

state_connect(internal, connect, #state{socket = Socket, host = Host, port = Port} = State) ->
  lager:info("Connecting to Host ~p", [{Host, Port}]),
  TransactionId = trunc(random:uniform() * 2147483647), % 32 bit int
  %% After sending every message wait for reply upto 10 * 2 ^n seconds with n starting from 0
  %% to 5
  DataBin = <<?PROTOCOL_ID:64, ?CONNECT_SEQ:32, TransactionId:32>>,
  ok = gen_udp:send(Socket, Host, Port, DataBin),
  {next_state, state_connecting, State#state{transaction_id = TransactionId, action=?CONNECT_SEQ },
    [15000]}.

state_connecting(timeout, _, #state{host=Host, action=Action} = State) ->
  lager:info("Didn't receive reply from tracker: ~p for action ~p in state_conn.Restarting...",[Host, Action]),
  %% shutdown and retry
  {stop, tracker_timeout};
state_connecting(info, {udp, _,  _, _, Msg}, #state{host=Host, action = Action, transaction_id = TransactionId} = State) ->
  lager:info("Received msg: ~p from tracker", [Msg]),
  16 = byte_size(Msg),
  <<Action:32, TransactionId:32, ConnectionId:64>> = Msg,
  lager:info("Received connection id ~p for ~p",[ConnectionId, Host]),
  {next_state, state_announce, State#state{connection_id = ConnectionId, connection_time = erlang:system_time(second)},
    [{next_event, internal, announce}]};
state_connecting(info, Msg, #state{host=Host, action=Action} = State) ->
  lager:info("Received unknown msg ~p from host:~p",[Msg, Host]),
  %% shutdown and retry
  {stop, tracker_unknown_msg}.

state_announce(internal, announce, #state{host=Host, port=Port, connection_id = ConnectionId, infohash = InfoHash,
  connection_time = ConnectionTime, peerid = PeerId, socket = Socket} = State) ->
  case is_connection_expired(ConnectionTime) of
    true -> {next_state, state_connect, State#state{connection_id=undefined}, [{next_event, internal, connect}]};
    false ->
      %% announce to tracker
      TransactionId = trunc(random:uniform() * 2147483647), % 32 bit int
      {ok, Torrent} = db_controller:update_with_repeat(fun db_controller:get_torrent/1, [InfoHash]),
      Dw = Torrent#torrents.downloaded,
      Left = Torrent#torrents.left,
      Uploaded = Torrent#torrents.uploaded,
      InfoHashBin = list_to_binary(InfoHash),
      PeerIdBin = list_to_binary(PeerId),
      Payload = <<ConnectionId:64, ?ANNOUNCE_SEQ:32, TransactionId:32,
        InfoHashBin/binary,
        PeerIdBin/binary,
        Dw:64, Left:64, Uploaded:64,
        0:32, 0:32, 0:32, -1:32, 6612:16 >>,
      lager:info("Sending announce to tracker: ~p",[Payload]),
      ok = gen_udp:send(Socket, Host, Port, Payload),
      {next_state, state_announcing, State#state{transaction_id = TransactionId, action=?ANNOUNCE_SEQ },
        [15000]}
  end.

state_announcing(timeout, _, #state{host=Host, action=Action} = _State) ->
  lager:info("Didn't receive reply from tracker: ~p for actiion ~p in state_announcing.Restarting...",[Host, Action]),
  {stop, tracker_timeout};
state_announcing(info, {udp, _,  _, _, Msg}, #state{host=Host, action = Action, transaction_id = TransactionId} = State) ->
  lager:info("Received valid msg from tracker in state_announcing"),
%%  16 = byte_size(Msg),
%%  <<Action:32, TransactionId:32, ConnectionId:64>> = Msg,
%%  lager:info("Received connection id ~p for ~p",[ConnectionId, Host]),
   {next_state, state_announcing, State};

state_announcing(info, Msg, #state{host=Host, action=Action} = State) ->
  lager:info("Received unknown msg ~p from host:~p in state_announcing",[Msg, Host]),
  {stop, tracker_unknown_msg}.

state_stop(internal, Reason, State) ->
  {stop, {shutdown, Reason}}.

state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

terminate(_Reason, _StateName, #state{host =Host, socket = Socket}) ->
  lager:info("Closing socket ~p for Uri ~p",[Socket, Host]),
  case Socket of
    undefined -> ok;
    _ ->   gen_udp:close(Socket)
  end.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_connection_expired(ConnectionTime) ->
  erlang:system_time(second) >= ConnectionTime +  ?CONNECTION_ID_LIFETIME.
