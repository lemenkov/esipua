%%%
%%% @doc       SIP REGISTER client supervisor
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(register_sup).

-behaviour(supervisor).

-include("siprecords.hrl").

%% api
-export([start_link/0, start_child/3, find_child/1]).

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


find_child(Aor) when is_list(Aor) ->
    Fun = fun({Id, _Pid, _Type, _Modules}) ->
		  Aor == Id
	  end,
    case lists:filter(Fun, supervisor:which_children(?SERVER)) of
	[] ->
	    error;
	[{_Id, Pid, _Type, _Modules}] ->
	    {ok, Pid}
    end.

%%--------------------------------------------------------------------
%% @spec start_child(Aor) -> Result
%%           Aor = contact()
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start client connection
%% @end
%%--------------------------------------------------------------------
start_child(Aor, Request, Owner) when is_record(Request, request),
				      is_pid(Owner) ->
    error_logger:info_msg("Start child ~p~n", [?MODULE]),
    ChildSpec = {Aor, {sipregister, start_link, [Request, Owner]},
		 temporary, 10000, worker, [sipregister]},
    supervisor:start_child(?SERVER, ChildSpec).


init(_Args) ->
    ServerSpec = {register_server, {register_server, start_link, []},
		permanent, 10000, worker, [register_server]},
    {ok, {{one_for_one, 10, 60}, [ServerSpec]}}.
