%%%
%%% @doc       Command decoder
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_decode).

-export([decode_command/1]).

-include("yate.hrl").

%%--------------------------------------------------------------------
%% @spec decode_command(Data) -> {ok, Dir, Cmd}
%%           Data = string()
%%           Dir = req | ans
%%           Cmd = #command{}
%% @doc Decode command string
%% @end
%%--------------------------------------------------------------------
decode_command(Data) ->
    {ok, {Type, Dir, Rest}} = decode_data(Data),
    {ok, Cmd} = decode_command(Type, Dir, Rest),
    {ok, Dir, Cmd}.

decode_command(install, ans, [PrioStr, NameStr, SuccessStr | KeyStrs]) ->
    Prio = decode_prio(PrioStr),
    Name = decode_name(NameStr),
    Success = decode_success(SuccessStr),
    Keys = command:decode_keys(KeyStrs),
    Header = #install{priority=Prio},
    {ok, #command{type=install,id=Name,success=Success,header=Header,keys=Keys}};
decode_command(uninstall, ans, [PrioStr, NameStr, SuccessStr | KeyStrs]) ->
    Prio = decode_prio(PrioStr),
    Name = decode_name(NameStr),
    Success = decode_success(SuccessStr),
    Keys = command:decode_keys(KeyStrs),
    Header = #uninstall{priority=Prio},
    {ok, #command{type=uninstall,id=Name,success=Success,header=Header,keys=Keys}};
decode_command(watch, ans, [NameStr, SuccessStr | KeyStrs]) ->
    Name = decode_name(NameStr),
    Success = decode_success(SuccessStr),
    Keys = command:decode_keys(KeyStrs),
    {ok, #command{type=watch,id=Name,success=Success,keys=Keys}};
decode_command(unwatch, ans, [NameStr, SuccessStr | KeyStrs]) ->
    Name = decode_name(NameStr),
    Success = decode_success(SuccessStr),
    Keys = command:decode_keys(KeyStrs),
    {ok, #command{type=unwatch,id=Name,success=Success,keys=Keys}};
decode_command(message, req, [IdStr, TimeStr, NameStr, RetStr | KeyStrs]) ->
    Id = decode_id(IdStr),
    Time = decode_time(TimeStr),
    Name = decode_name(NameStr),
    Keys = command:decode_keys(KeyStrs),
    Header = #message{time=Time,name=Name,retvalue=RetStr},
    {ok, #command{type=message,id=Id,header=Header,keys=Keys}};
decode_command(message, ans, [IdStr, ProcessedStr, NameStr, RetStr | KeyStrs]) ->
    Id = decode_id(IdStr),
    Success = decode_processed(ProcessedStr),
    Name = decode_name(NameStr),
    Keys = command:decode_keys(KeyStrs),
    Header = #message{name=Name,retvalue=RetStr},
    {ok, #command{type=message,id=Id,header=Header,success=Success,keys=Keys}}.

decode_prio(Str) ->
    list_to_integer(Str).

decode_processed(Str) ->
    list_to_boolean(Str).

list_to_boolean("true") ->
    true;
list_to_boolean("false") ->
    false.

decode_success(Str) ->
    list_to_atom(Str).

decode_id(IdStr) ->
    %%list_to_atom(IdStr).
    IdStr.

decode_time(TimeStr) ->
    list_to_integer(TimeStr).

decode_name(NameStr) ->
    list_to_atom(NameStr).

decode_data(Data) ->
    "%%" ++ Body = string:strip(Data, right, $\n),
    [First|RestEsc] = split(Body, $:),
    Rest = unescape(RestEsc),
    [DirChr|TypeStr] = First,
     Dir = decode_dir([DirChr]),
     Type = decode_type(TypeStr),
    {ok, {Type, Dir, Rest}}.


split(List, Char) ->
    split(List, Char, "", []).

split([], _Char, Str, Res) ->
    Res ++ [Str];
split([Char|R], Char, Str, Res) ->
    split(R, Char, "", Res ++ [Str]);
split([C|R], Char, Str, Res) ->
    split(R, Char, Str ++ [C], Res).

unescape(List) ->
    unescape_list(List, []).

unescape_list([], Res) ->
    Res;
unescape_list([Item|R], Res) ->
    unescape_list(R, Res ++ [unescape_string(Item, [])]).

unescape_string("", Res) ->
    Res;
unescape_string("%%" ++ R, Res) ->
    unescape_string(R, Res ++ "%");
unescape_string([$\%, Upcode | R], Res) ->
    C = Upcode - 64,
    unescape_string(R, Res ++ [C]);
unescape_string([C|R], Res) ->
    unescape_string(R, Res ++ [C]).

decode_type(TypeStr) ->
    list_to_atom(TypeStr).

decode_dir(">") ->
    req;
decode_dir("<") ->
    ans.

