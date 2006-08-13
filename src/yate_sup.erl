%%%
%%% @doc       Supervisor module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_sup).

-behaviour(supervisor).

%% api
-export([start_link/0, start_conn/3, start_client/2]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_conn(Host, Port, Pid) ->
    Id = {conn, Host, Port, Pid, make_ref()},
    ChildSpec = {Id, {yate_conn_srv, start_link, [Host, Port, Pid]},
		 temporary, 60, worker, [yate_conn_srv]},
    Res = supervisor:start_child(?SERVER, ChildSpec),
    Res.

start_client(Host, Port) ->
    Id = {client, Host, Port, make_ref()},
    ChildSpec = {Id, {yate_srv, start_link, []},
		 temporary, 10, worker, [yate_srv]},
    {ok, Pid} = supervisor:start_child(?SERVER, ChildSpec),
    ok = yate_srv:connect(Pid, Host, Port),
    {ok, Pid}.

init(_Args) ->
    RegSrvSpec = {yate_reg_srv, {yate_reg_srv, start_link, []},
		 permanent, 10, worker, [yate_reg_srv]},
    {ok, {{one_for_one, 10, 60}, [RegSrvSpec]}}.

find_child(Id) ->
    ChildSpecs = supervisor:which_children(?SERVER),
    case [{IdX, ChildX, TypeX, ModuleX} ||
	     {IdX, ChildX, TypeX, ModuleX} <- ChildSpecs,
	     IdX == Id] of
	[] ->
	    error;
	[{Id, Child, Type, Module}] ->
	    {ok, {Id, Child, Type, Module}}
    end.
