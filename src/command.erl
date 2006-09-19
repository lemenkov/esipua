-module(command).

-include("yate.hrl").

-export([find_key/2, fetch_key/2]).

find_key(Key, Cmd) ->
    dict:find(Key, Cmd#command.keys).

fetch_key(Key, Cmd) ->
    dict:fetch(Key, Cmd#command.keys).
