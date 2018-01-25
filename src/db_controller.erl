%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2018 5:51 PM
%%%-------------------------------------------------------------------
-module(db_controller).
-author("saurav").

-include("../include/torrer.hrl").
-define(META, meta).
-define(TORRENTS, torrents).
-define(PEER_ID_PREFIX, "-TE1001-").
-define(DEFAULT_REPEATS, 2).

%% API
-export([
  update_with_repeat/2,
  update_with_repeat/3,
  get_and_update_peer_id/0,
  set_peer_id/1,
  check_downloaded/1,
  get_meta/1,
  add_meta/2,
  add_torrent/1,
  get_torrent/1
  ]).

update_with_repeat(Fun, Args) ->
  update_with_repeat(Fun, Args, ?DEFAULT_REPEATS).

update_with_repeat(Fun, Args, 0) ->
  lager:error("Max retries reached while calling Fun: ~p with Args: ~p", [Fun, Args]),
  {error, max_reties};
update_with_repeat(Fun, Args, Repeat) ->
  case apply(Fun, Args) of
    ok -> ok;
    {ok, _} = R -> R;
    {error, E} when is_list(E) -> update_with_repeat(Fun, Args, Repeat - 1);
    {error, _}  = E -> E
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All functions called via update with repeat should return
% ok, {ok, Term}, {error, Term}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_meta(term()) -> term().
get_meta(K) ->
  Fun = fun() ->
      mnesia:read({?META, K})
    end,
  case mnesia:transaction(Fun) of
    {atomic, []} ->
      lager:info("Key ~p not found in meta table",[K]),
      {error, mnesia_key_not_found};
    {atomic, [V]} -> {ok, V};
    {aborted, Reason} ->
      lager:info("Error while reading Key ~p from meta table: ~p", [K, Reason]),
      {error, Reason}
  end.

-spec add_meta(term(), term()) -> ok | {error, term()}.
add_meta(K, V) ->
  Record = #meta{key = K, value = V},
  Fun = fun() ->
    mnesia:write(Record)
  end,
  case mnesia:transaction(Fun) of
    {atomic, ok} -> ok;
    {aborted, Reason} ->
      lager:error("Aborted mnesia meta table updata for ~p with ~p:~n~p",[K, V, Reason]),
      {error, Reason}
  end.

get_torrent(InfoHash) ->
  Fun = fun() ->
    mnesia:read({?TORRENTS, InfoHash})
        end,
  case mnesia:transaction(Fun) of
    {atomic, []} -> {error, torrent_not_found};
    {atomic, [Record]} -> {ok, Record};
    {aborted, Reason} ->
      lager:error("Error while checking if torrent ~p is already downloaded: ~p", [InfoHash, Reason]),
      {error, Reason}
  end.

add_torrent(Torrent) ->
  Fun = fun() ->
    mnesia:write(Torrent)
  end,
  case mnesia:transaction(Fun) of
    {atomic, ok} -> ok;
    {aborted, Reason} ->
      lager:error("Error while adding torrent record ~p to torrent table: ~p", [Torrent, Reason]),
      {error, Reason}
  end.

-spec check_downloaded(list()) -> {ok, true} | {ok, false} | {error, term()}.
check_downloaded(InfoHash) ->
  Fun = fun() ->
      mnesia:read({?TORRENTS, InfoHash})
    end,
  case mnesia:transaction(Fun) of
    {atomic, []} -> {ok, false};
    {atomic, [Record]} -> {ok, Record#torrents.done};
    {aborted, Reason} ->
      lager:error("Error while checking if torrent ~p is already downloaded: ~p", [InfoHash, Reason]),
      {error, Reason}
  end.

-spec get_and_update_peer_id() -> {ok, list()} | {error, term()}.
get_and_update_peer_id() ->
  GetPeer = fun() -> mnesia:read({?META, peer_id}) end,
  case mnesia:transaction(GetPeer) of
    {atomic, [Rec|_]} ->
      PeerId = Rec#?META.value,
      lager:info("Restoring peer id ~p from table", [PeerId]),
      {ok, PeerId};
    {atomic, []} ->
      lager:info("Peerid not found in table, generating one"),
      NewPeerId = ?PEER_ID_PREFIX ++ lists:sublist(binary_to_list(base64:encode(crypto:strong_rand_bytes(100))), 12),
      lager:info("Generated new peer id ~p~n", [NewPeerId]),
      ok = set_peer_id(NewPeerId),
      {ok, NewPeerId};
    {aborted, Reason} ->
      lager:error("Error while getting peer_id ~p~n", [Reason]),
      {error, Reason}
  end.

-spec set_peer_id(list()) -> ok | {error, term()}.
set_peer_id(PeerId) ->
  Record = #?META{key=peer_id, value=PeerId},
  SetPeer = fun() -> mnesia:write(Record) end,
  case mnesia:sync_transaction(SetPeer) of
    {atomic, ok} -> ok;
    {aborted, Reason} ->
      lager:error("Error while setting peer_id ~p~n~p~n", [PeerId, Reason]),
      {error,Reason}
  end.

