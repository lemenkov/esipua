-module(key_dict).

-export([encode/1, decode/1]).


encode(Keys) ->
    yate_encode:encode_dict(key, Keys).


decode(KeyStrs) ->
    decode(KeyStrs, undefined).

decode([], Keys) ->
    Keys;
decode([KeyStr|R], Keys) ->
    {NameStr, Value} = decode_key(KeyStr),
    Name = list_to_atom(NameStr),
    case Keys of
	undefined ->
	    NewKeys = dict:new();
	_ ->
	    NewKeys = Keys
    end,
    decode(R, dict:store(Name, Value, NewKeys)).

decode_key(KeyStr) ->
    decode_key(KeyStr, []).

decode_key([$=|Value], Name) ->
    {Name, Value};
decode_key([C|R], Name) ->
    decode_key(R, Name ++ [C]).

