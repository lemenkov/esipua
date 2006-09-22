%%%
%%% @doc       Command encoder
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_encode).

-export([encode_command/3, encode_dict/2, encode_list/2]).

-include("yate.hrl").

%%--------------------------------------------------------------------
%% @spec encode_command(Type, Dir, Cmd) -> string()
%%           Type = connect|install|uninstall|watch|unwatch|message
%%           Dir = req | ans
%%           Cmd = #command{}
%% @doc Encode command into string
%% @end
%%--------------------------------------------------------------------
encode_command(connect, req, Cmd) ->
    Header = Cmd#command.header,
    Headers = [Header#connect.role, Cmd#command.id, Header#connect.type],
    encode_data({connect, req, Headers, Cmd#command.keys});
encode_command(install, req, Cmd) ->
    Header = Cmd#command.header,
    Filter = encode_filter(Header#install.filter),
    Headers = [Header#install.priority, Cmd#command.id, Filter],
    encode_data({install, req, Headers, Cmd#command.keys});
encode_command(uninstall, req, Cmd) ->
    Headers = [Cmd#command.id],
    encode_data({uninstall, req, Headers, Cmd#command.keys});
encode_command(watch, req, Cmd) ->
    Headers = [Cmd#command.id],
    encode_data({watch, req, Headers, Cmd#command.keys});
encode_command(unwatch, req, Cmd) ->
    Headers = [Cmd#command.id],
    encode_data({unwatch, req, Headers, Cmd#command.keys});
encode_command(message, ans, Cmd) ->
    Header = Cmd#command.header,
    Headers = [Cmd#command.id, Cmd#command.success, Header#message.name, Header#message.retvalue],
    encode_data({message, ans, Headers, Cmd#command.keys});
encode_command(message, req, Cmd) ->
    Header = Cmd#command.header,
    Headers = [Cmd#command.id, Header#message.time, Header#message.name, Header#message.retvalue],
    encode_data({message, req, Headers, Cmd#command.keys}).

encode_filter(undefined) ->
    "".

encode_data({Type, Dir, Headers, Keys}) ->
    {ok, TypeStr} = encode_type(Type),
    {ok, DirStr} = encode_dir(Dir),
    HeaderStr = encode_params(header, Headers),
    KeyStr = command:encode_keys(Keys),
    Msg = "%%" ++ DirStr ++ TypeStr ++ HeaderStr ++ KeyStr ++ "\n",
    {ok, Msg}.

encode_type(Type) ->
    {ok, atom_to_list(Type)}.

encode_dir(req) ->
    {ok, ">"};
encode_dir(ans) ->
    {ok, "<"}.

encode_params(Type, Params) ->
    encode_params(Type, Params, []).

encode_params(_Type, [], Res) ->
    Res;
encode_params(Type, [H|R], Res) ->
    ParamStr = encode_param(Type, H),
    encode_params(Type, R, Res ++ ":" ++ ParamStr).

encode_dict(_Type, undefined) ->
    [];
encode_dict(Type, Dict) ->
    Fun = fun(Key, Value, AccIn) -> encode_dict_item(Key, Value, AccIn) end,
    {Type, Res} = dict:fold(Fun, {Type, []}, Dict),
    Res.

encode_list(_Type, undefined) ->
    [];
encode_list(Type, List) ->
    Fun = fun({Key, Value}, AccIn) -> encode_dict_item(Key, Value, AccIn) end,
    {Type, Res} = lists:foldl(Fun, {Type, []}, List),
    Res.

encode_dict_item(Key, Value, {Type, Res}) ->
    ParamStr = encode_param(Type, {Key, Value}),
    {Type, Res ++ ":" ++ ParamStr}.

encode_param(header, Param) ->
    encode_string(encode_value(Param));
encode_param(key, {Name, Value}) ->
    atom_to_list(Name) ++ "=" ++ encode_string(encode_value(Value)).

encode_value(Value) ->
    if is_integer(Value) ->
	    integer_to_list(Value);
       is_atom(Value) ->
	    atom_to_list(Value);
       is_reference(Value) ->
	    erlang:ref_to_list(Value);
       is_list(Value) ->
	    Value
    end.

encode_string([Char|R]) ->
    Enc =
	if
	    Char < 32; Char == $:; Char == $\% ->
		[$\%, Char + 64];
	    true ->
		Char
    end,
    [Enc | encode_string(R)];
encode_string([]) ->
    [].
