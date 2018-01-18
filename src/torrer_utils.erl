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
  get_uuid/0
]).

get_uuid()->
  list_to_atom(uuid:to_string(uuid:uuid4())).