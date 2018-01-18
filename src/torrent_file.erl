%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2018 8:12 PM
%%%-------------------------------------------------------------------
-module(torrent_file).
-author("saurav").

%% API
-export([get_trackers/1, get_trackers/2]).

get_trackers(TorrentDict) ->
 get_trackers(TorrentDict, all).

get_trackers(TorrentDict, all)->
 get_trackers(TorrentDict, udp) ++ get_trackers(TorrentDict, http);
get_trackers(TorrentDict, udp) ->
 AnnounceList = maps:get(<<"announce-list">>, TorrentDict, [[maps:get(<<"announce">>, TorrentDict)]]),
 lists:filter(fun(E) ->
                        case E of
                          <<"udp://", _T/binary>> -> true;
                          _ -> false
                        end end, lists:flatten(AnnounceList));
get_trackers(TorrentDict, http) ->
  AnnounceList = maps:get(<<"announce-list">>, TorrentDict, [[maps:get(<<"announce">>, TorrentDict)]]),
  lists:filter(fun(E) -> case E of
                           <<"http://", _T/binary>> -> true;
                           _ -> false
                         end end, lists:flatten(AnnounceList)).
