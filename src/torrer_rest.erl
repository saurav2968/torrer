%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jan 2018 10:39 AM
%%%-------------------------------------------------------------------
-module(torrer_rest).
-author("saurav").

%% API
-export([
  init/3,
  allowed_methods/2,
  content_types_provided/2,
  handle_get/2,
  content_types_accepted/2,
  handle_post/2
]).

init({tcp, http}, Req, Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% methods should return {Value, Req, State}
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"DELETE">>], Req, State}.

%% GET and HEAD handler
content_types_provided(Req, State) ->
  Handlers = [{<<"application/json">>, handle_get}],
  {Handlers, Req, State}.

content_types_accepted(Req, State) ->
  Accepted = [{{<<"application">>, <<"json">>, '*'}, handle_post}],
  {Accepted, Req, State}.


%%%% Handlers

handle_get(Req, State) ->
  lager:info("GET request: ~p", [Req]),
  Body = <<"">>,
  {Body, Req, State}.

handle_post(Req, State) ->
  {Path, Req1} = cowboy_req:path(Req),
  {ok, Body, Req2} = cowboy_req:body(Req1),
  %% POST request on path: <<"/rest/new">> with body: <<"file=C%3A%5Cfakepath%5CMastiTorrents.Net.txt">>
  lager:info("POST request on path: ~p with body: ~p", [Path, Body]),
  request_fsm_sup:new_request(binary_to_list(Body)),
  {true, Req2, State}.
