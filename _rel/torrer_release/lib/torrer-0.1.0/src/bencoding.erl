%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 11:33 PM
%%%-------------------------------------------------------------------
-module(bencoding).
-author("saurav").
-include_lib("stdlib/include/assert.hrl").
%% API
-export([decode/1]).

decode(Payload) when is_binary(Payload)->
  Payload2 = binary_to_list(Payload),
  decode(Payload2);

decode(Payload) when is_list(Payload) ->
  case Payload of
    [$d | T] ->
      {ok, decode_dict(T, #{})};
    [In | _] ->
      lager:error("Bad starting index ~p in torrent file", [In]),
      error(bad_torrent)
  end.

decode_value(Payload) ->
  case Payload of
    [$d | T] -> decode_dict(T, #{});
    [$l | T] -> decode_list(T, []);
    [$i | T] -> decode_int(T);
    String   -> decode_string(String)
  end.

decode_dict([$e | T], Acc) ->
  {Acc, T};
decode_dict(Payload, Acc) ->
  {Key, Payload2} = decode_string(Payload),
  lager:info("Key in dict ~p",[Key]),
  {Value, Payload3} = decode_value(Payload2),
  lager:info("Value for key: ~p", [Value]),
  Acc2 = maps:put(Key, Value, Acc),
  decode_dict(Payload3, Acc2).

decode_list([$e | T], Acc) ->
  {Acc, T};
decode_list(Payload, Acc) ->
  {Val, Payload2} = decode_value(Payload),
  decode_list(Payload2, Acc ++ [Val]).

decode_int(Payload) ->
  Index = string:chr(Payload, $e),
  lager:info("Index of e in int is ~p", [Index]),
  case Index of
    0 ->
      lager:error("Bad payload- Unable to find trailing e for integer: ~p", [Payload]),
      error({bad_payload, int});
    _ ->
      IntAsString = string:sub_string(Payload, 1, Index -1),
      lager:info("IntAsString is ~p", [IntAsString]),
      case catch list_to_integer(IntAsString) of
        {'EXIT', Reason} ->
          lager:error("Didn't find int in ~p: ~p", [IntAsString, Reason]),
          error({bad_payload, int});
        Int -> {Int, string:sub_string(Payload, Index + 1)}
      end
  end.

decode_string(Payload) ->
  List = string:split(Payload, ":"),
  ?assert(length(List) > 1),
  [IntAsString, Payload2] = List,
  case catch list_to_integer(IntAsString) of
    {'EXIT', Reason} ->
      lager:error("Didn't find int in ~p: ~p", [IntAsString, Reason]),
      error({bad_payload, int});
    Int ->
      {string:sub_string(Payload2, 1, Int), string:sub_string(Payload2, Int + 1)}
  end.

%%%%%%%% string l(Payload) ->
% verify_norm(Payload, {0, 0}).

%%verify_norm([C | T], {DPlusLCount, false}) ->
%%  case C of
%%    $d -> verify_norm(T, DPlusLCount + 1, false);
%%    $l -> verify_norm(T, DPlusLCount +1, false);
%%    $i -> a
%%  end.