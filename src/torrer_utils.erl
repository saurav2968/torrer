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

%% API
-export([
  get_uuid/0,
  get_host_and_port_from_uri/1
]).

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
