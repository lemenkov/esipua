%%%
%%% @doc       yate_demo top supervisor
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo_sup).

-behaviour(supervisor).

%% api
-export([start_link/0, start_child/1]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).

-define(HOST, localhost).
-define(PORT, 15062).


%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%--------------------------------------------------------------------
%% @spec start_child(ChildSpec) -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start child
%% @end
%%--------------------------------------------------------------------
start_child(ChildSpec) ->
    supervisor:start_child(?SERVER, ChildSpec).

init(_Args) ->
    ChildSpec = {yate_demo, {yate_demo, start_link, []},
		 permanent, 10, worker, [yate_demo]},
    {ok, {{one_for_one, 10, 60}, [ChildSpec]}}.
