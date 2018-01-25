%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 9:29 PM
%%%-------------------------------------------------------------------
-module(torrer_utils).
-author("saurav").

-include("../include/torrer.hrl").
%% API
-export([
  get_uuid/0,
  get_host_and_port_from_uri/1,
  get_torrent_record/2,
  get_infohash/1
]).

is_single_file(TorrentDict) ->
  maps:is_key(<<"length">>, maps:get(<<"info">>, TorrentDict)).

get_info_single(TorrentDict) ->
  InfoDict = maps:get(<<"info">>, TorrentDict),
  {NP, TotalSize, PiecesHash} = get_common_info(InfoDict),
  Name = maps:get(<<"name">>, InfoDict),
  Length = maps:get(<<"length">>, InfoDict),
  {NP, TotalSize, PiecesHash, [{Name, Length}]}.

get_info_multiple(TorrentDict) ->
  InfoDict = maps:get(<<"info">>, TorrentDict),
  {NP, TotalSize, PiecesHash} = get_common_info(InfoDict),
  lager:info("KEYS: ~p", [maps:keys(InfoDict)]),
  Files = lists:foldl(fun(E, Acc) ->
      Acc ++ [{lists:foldl(fun(E, Acc) -> Acc ++ "/" ++ binary_to_list(E) end, "", maps:get(<<"path">>, E)), maps:get(<<"path">>, E)}]
    end, [], maps:get(<<"files">>, InfoDict)),
  {NP, TotalSize, PiecesHash, Files}.

get_common_info(InfoDict)->
  lager:info("InfoDict keys is ~p", [maps:keys(InfoDict)]),
  PieceL = maps:get(<<"piece length">>,  InfoDict),
  PiecesHash = maps:get(<<"pieces">>,  InfoDict),
  NumPieces = byte_size(PiecesHash) / 20,
  {NumPieces, NumPieces * PieceL, PiecesHash}.

get_uuid()->
  list_to_atom(uuid:to_string(uuid:uuid4())).

%% Uri -> "udp://62.138.0.158/announce"
get_host_and_port_from_uri(Uri) ->
  HostAndPort = lists:nth(3, string:split(Uri, "/", all)),
  L = string:split(HostAndPort, ":"),
  case length(L) of
    1 -> {error, missing_udp_port};
    2 -> {hd(L), list_to_integer(lists:nth(2, L))}
  end.

get_torrent_record(InfoHash, TorrentDict) ->
  {NP, TotalSize, PiecesHash, Files} = case is_single_file(TorrentDict) of
    true -> get_info_single(TorrentDict);
    false -> get_info_multiple(TorrentDict)
  end,
  #torrents{
    info_hash=InfoHash,
    done = false,
    uploaded = 0,
    downloaded = 0,
    left = trunc(TotalSize),
    num_pieces = NP,
    pieces_hash = PiecesHash,
    files = Files,
    pieces_remaining = NP,
    when_started = erlang:system_time(second),
    when_done = undefined,
    total_size = TotalSize, %% this is float
    download_dir = "/home/saurav/torrer/"
  }.

rfc_3986_unreserved_characters() ->
  % jlouis: I deliberately killed ~ from the list as it seems the Mainline
  %  client doesn't announce this.
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_./".

rfc_3986_unreserved_characters_set() ->
  sets:from_list(rfc_3986_unreserved_characters()).

%% @doc Convert the list into RFC1738 encoding (URL-encoding).
%% @end
-spec build_encoded_form_rfc1738(string() | binary()) -> string().
build_encoded_form_rfc1738(List) when is_list(List) ->
  Unreserved = rfc_3986_unreserved_characters_set(),
  F = fun (E) ->
    case sets:is_element(E, Unreserved) of
      true ->
        E;
      false ->
        lists:concat(
          ["%", io_lib:format("~2.16.0B", [E])])
    end
      end,
  lists:flatten([F(E) || E <- List]);
build_encoded_form_rfc1738(Binary) when is_binary(Binary) ->
  build_encoded_form_rfc1738(binary_to_list(Binary)).

get_infohash(Payload) ->
  crypto:hash(sha, Payload).
  %build_encoded_form_rfc1738(crypto:hash(sha, Payload)).
%%  << << if N >= 10 -> N -10 + $a;
%%                    true    -> N     + $0 end >>
%%      || <<N:4>> <= crypto:hash(sha, Payload) >>.
%%  %io_lib:format("<<~s>>~n", [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= crypto:hash(sha, Payload) ]]).

