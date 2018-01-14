%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jan 2018 11:43 PM
%%%-------------------------------------------------------------------
-module(request_listener).
-author("saurav").

%% API
-export([start_link/0]).

start_link() ->
  init().

init() ->
  lager:info("Starting request_listener..."),
  {Pid, Value} = gproc:await({n, l, bootstrap_fsm}, 2000),
  true = is_pid(Pid),
  lager:info("Bootstrap done with value ~p~n", [Value]),
  timer:sleep(1000),
  loop().

loop() ->
  case io:fread("Enter torrent file path> ", "~s") of
    {ok, [FileName]} ->
      lager:info("Processing torrent file: ~p", [FileName]),
      %% User needs to press Enter to display prompt
      request_fsm_sup:new_request(FileName),
      io:get_line(""),
      loop();
    eof ->
      lager:info("User asked to abort!"),
      init:stop(0)
  end.




