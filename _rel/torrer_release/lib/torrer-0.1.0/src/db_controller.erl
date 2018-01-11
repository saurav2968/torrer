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
-define(PEER_ID_PREFIX, "-TE0101-").

%% API
-export([get_and_update_peer_id/0, set_peer_id/1]).

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

