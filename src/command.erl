-module(command).

-exports(find_key/1, fetch_key/1).

find_key(Key, Cmd) ->
    dict:find(Key, Cmd#command.keys).

fetch_key(Key, Cmd) ->
    dict:fetch(Key, Cmd#command.keys).
