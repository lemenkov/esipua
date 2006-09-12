%%
%% Yate sip server connection manager
%%

-module(ysip_srv).

-behaviour(gen_server).

%% api
-export([start_link/2, stop/0, invite/2]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {client, handle}).

-define(SERVER, ?MODULE).

start_link(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

stop() ->
    gen_server:cast(?SERVER, stop).

invite(Request, LogStr) ->
    Client = gen_server:call(?SERVER, get_client),
    sipclient:start_link(Client, Request, LogStr).
    

%%
%% gen_server callbacks
%%
init([Host, Port]) ->
    {ok, Client} = yate:connect(Host, Port),
    {ok, Handle} = yate:open(Client),
    {ok, #state{client=Client, handle=Handle}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% handle_call({invite, Request, LogStr}, _From, State) ->
%%     {ok, _Pid} = sipclient:start_link(State#state.client, Request, LogStr),
%%     {reply, ok, State};
handle_call(get_client, _From, State) ->
    {reply, State#state.client, State};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.
