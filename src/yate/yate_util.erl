%%%
%%% @doc       Yate utilities
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%

-module(yate_util).

-export([seconds/0, dict_store_list/2]).

-include("yate.hrl").

%%--------------------------------------------------------------------
%% @spec seconds() -> integer()
%% @doc Return number of seconds from epoch
%% @end
%%--------------------------------------------------------------------
seconds() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000000 + Secs.


dict_store_list([], Dict) ->
    Dict;
dict_store_list([{Key, Value}|R], Dict) ->
    dict_store_list(R, dict:store(Key, Value, Dict)).
