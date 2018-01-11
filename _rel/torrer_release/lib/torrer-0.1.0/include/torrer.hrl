%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2018 5:08 PM
%%%-------------------------------------------------------------------
-author("saurav").

-record(meta, {key, value}).

-record(torrents, {info_hash, done, uploaded, downloaded, left, num_pieces, pieces_hash, files, pieces_remaining, when_started,
  when_done, total_size, download_dir}).

-record(peers, {peer_id, ip, port, pieces_have}). %% is this neede or maybe just save in peer state and make it
% avaialble via messages

-record(choke, {type, peers}).