-module(command).

-include("yate.hrl").

-define(KEY_IMPL, key_list).

-export([find_key/2, fetch_key/2, encode_keys/1, decode_keys/1,
	 append_keys/2]).

find_key(Key, Cmd) ->
    ?KEY_IMPL:find(Key, Cmd#command.keys).

fetch_key(Key, Cmd) ->
    ?KEY_IMPL:fetch(Key, Cmd#command.keys).

append_keys(Keys, Cmd) ->
    Cmd#command{keys=?KEY_IMPL:append(Keys, Cmd#command.keys)}.

encode_keys(Cmd) when is_record(Cmd, command) ->
    encode_keys(Cmd#command.keys);
encode_keys(Keys) ->
    ?KEY_IMPL:encode(Keys).

decode_keys(KeyStr) ->
    ?KEY_IMPL:decode(KeyStr).
