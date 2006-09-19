-module(command).

-include("yate.hrl").

-export([find_key/2, fetch_key/2, encode_keys/1, decode_keys/1]).

find_key(Key, Cmd) ->
    dict:find(Key, Cmd#command.keys).

fetch_key(Key, Cmd) ->
    dict:fetch(Key, Cmd#command.keys).


encode_keys(Cmd) when is_record(Cmd, command) ->
    encode_keys(Cmd#command.keys);
encode_keys(Keys) ->
    key_dict:encode(Keys).

decode_keys(KeyStr) ->
    key_dict:decode(KeyStr).
