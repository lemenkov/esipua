-module(yate_call).

-behaviour(gen_server).

-include("yate.hrl").

%% api
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  client,
	  handle,
	  parent,
	  id
	 }).

start_link(Client) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Client, self()], []),
    {ok, Pid}.

stop(Call) ->
    gen_server:cast(Call, stop).

%%
%% gen_server callbacks
%%
init([Client, Parent]) ->
    {ok, Handle} = yate:open(Client),
    State = #state{client=Client,handle=Handle,parent=Parent},
    ok = setup_watches(State),
    {ok, State}.

setup_watches(State) ->
    Handle = State#state.handle,
    Id = State#state.id,
    ok = yate:watch(Handle, chan.disconnected,
		    fun(Cmd) ->
			    Id == command:fetch_key(id, Cmd)
		    end),
    ok = yate:watch(Handle, call.ringing,
		    fun(Cmd) ->
 			    Id == command:fetch_key(targetid, Cmd)
		    end),

    ok = yate:watch(Handle, chan.hangup,
		    fun(Cmd) ->
			    Id == command:fetch_key(id, Cmd)
		    end),
    ok = yate:watch(Handle, call.progress,
		    fun(Cmd) ->
			    Id == command:fetch_key(targetid, Cmd)
		    end),
    ok = yate:watch(Handle, call.answered,
		    fun(Cmd) ->
			    Id == command:fetch_key(targetid, Cmd)
		    end),
    ok = yate:watch(Handle, call.drop,
		    fun(Cmd) ->
			    %% Check
			    Id == command:fetch_key(targetid, Cmd)
		    end),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({'EXIT', Pid, Reason}, State=#state{parent=Pid}) ->
    {stop, Reason, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    {stop, Reason, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#state.handle,
    yate_srv:close(Handle),
    terminated.
