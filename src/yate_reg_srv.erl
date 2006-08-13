-module(yate_reg_srv).

-behaviour(gen_server).

%% api
-export([start_link/0, stop/0, get_conn/2]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(sstate, {connections=dict:new()}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

get_conn(Host, Port) ->
    Pid = self(),
    gen_server:call(?SERVER, {get_conn, Host, Port, Pid}).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, #sstate{}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({get_conn, Host, Port, Pid}, _From, State) ->
    Id = {conn, Host, Port},
    case dict:find(Id, State#sstate.connections) of
	{ok, OldPid} ->
	    {reply, OldPid, State};
	error ->
	    NewPid = yate_conn:connect(Host, Port, Pid),
	    NewConnections = dict:store(Id, NewPid, State#sstate.connections),
	    {reply, Pid, State#sstate{connections = NewConnections}}
    end;
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unsupported call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unsupported cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Info, State) ->
    error_logger:error_msg("Unsupported info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.
