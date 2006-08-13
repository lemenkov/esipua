%%%
%%% @doc       TCP connection server
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%

-module(yate_util).

-export([seconds/0]).

-include("yate.hrl").

%%--------------------------------------------------------------------
%% @spec seconds() -> integer()
%% @doc Return number of seconds from epoch
%% @end
%%--------------------------------------------------------------------
seconds() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000000 + Secs.
