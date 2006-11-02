%%%
%%% @doc       Top supervisor
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_port_sup).

-behaviour(supervisor).

%% api
-export([start_link/0, start_client/2]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%--------------------------------------------------------------------
%% @spec start_client(Host, Port) -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start client connection
%% @end
%%--------------------------------------------------------------------
start_client(Host, Port) ->
    error_logger:info_msg("Start child ~p~n", [?MODULE]),
    Id = {yate_srv, Host, Port, make_ref()},
    ChildSpec = {Id, {yate_srv, start_link, [Host, Port]},
		 temporary, 10000, worker, [yate_srv]},
    supervisor:start_child(?SERVER, ChildSpec).


init(_Args) ->
    ok = error_logger:logfile({open, "yate.log"}),
    PortSpec = {yate_port_srv, {yate_port_srv, start_link, []},
		permanent, 10000, worker, [yate_port_srv]},
    CallRegSpec = {yate_call_reg, {yate_call_reg, start_link, []},
		   permanent, 10000, worker, [yate_call_reg]},
    {ok, {{one_for_one, 10, 60}, [PortSpec, CallRegSpec]}}.
