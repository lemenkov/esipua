-module(key_list).

-export([find/2, fetch/2, encode/1, decode/1]).


find(Key, Keys) ->
    case lists:keysearch(Key, 1, Keys) of
	{value, {Key, Value}} ->
	    {ok, Value};
	false ->
	    error
    end.


fetch(Key, Keys) ->
    case lists:keysearch(Key, 1, Keys) of
	{value, {Key, Value}} ->
	    Value;
	false ->
	    exit({not_found, Key, Keys})
    end.


encode(Keys) ->
    yate_encode:encode_list(key, Keys).


decode(KeyStrs) ->
    decode(KeyStrs, undefined).

decode([], Keys) ->
    Keys;
decode([KeyStr|R], Keys) ->
    {NameStr, Value} = decode_key(KeyStr),
    Name = list_to_atom(NameStr),
    case Keys of
	undefined ->
	    NewKeys = [];
	_ ->
	    NewKeys = Keys
    end,
    decode(R, [{Name, Value}|NewKeys]).

decode_key(KeyStr) ->
    decode_key(KeyStr, []).

decode_key([$=|Value], Name) ->
    {Name, Value};
decode_key([C|R], Name) ->
    decode_key(R, Name ++ [C]).

