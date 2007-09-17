%%%-------------------------------------------------------------------
%%% File    : sipua_sup.erl
%%% Author  : Mikael Magnusson <mikma@users.sourceforge.net>
%%% Description : 
%%%
%%% Created : 30 Oct 2006 by Mikael Magnusson <mikma@users.sourceforge.net>
%%%-------------------------------------------------------------------
-module(sipua_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Callregister = {callregister, {callregister, start_link, []},
                    permanent, 2000, worker, [callregister]},
    RegisterSupSpec = {register_sup, {register_sup, start_link, []},
		       permanent, 2000, worker, [register_sup]},
    {ok,{{one_for_one,0,1}, [Callregister, RegisterSupSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
