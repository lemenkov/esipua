%% TODO link call controller

-module(callregister).

-behaviour(gen_server).

%% api
-export([
	 start_link/0,
	 stop/0,
	 register_call/2,
	 unregister_call/1,
	 find_call/1,
	 dump/0
	]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  calls=dict:new()
	 }).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    {ok, Pid}.

stop() ->
    gen_server:cast(?SERVER, stop).

register_call(CallId, Pid) ->
    gen_server:call(?SERVER, {register_call, CallId, Pid}).
    
unregister_call(CallId) ->
    gen_server:call(?SERVER, {unregister_call, CallId}).

find_call(CallId) ->
    gen_server:call(?SERVER, {find_call, CallId}).

dump() ->
    gen_server:cast(?SERVER, dump).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, #state{}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({register_call, CallId, Pid}, _From, State) ->
    Calls = dict:store(CallId, Pid, State#state.calls),
    State1 = State#state{calls=Calls},
    {reply, ok, State1};

handle_call({unregister_call, CallId}, _From, State) ->
    Calls = dict:erase(CallId, State#state.calls),
    State1 = State#state{calls=Calls},
    {reply, ok, State1};

handle_call({find_call, CallId}, _From, State) ->
    Result = dict:find(CallId, State#state.calls),
    {reply, Result, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(dump, State) ->
    io:format("callregister ~n~p", [dict:to_list(State#state.calls)]),
    {noreply, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.
