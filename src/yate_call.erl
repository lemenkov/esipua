-module(yate_call).

-behaviour(gen_server).

-include("yate.hrl").

%% api
-export([start_link/2, answer/1, drop/2, drop/1, play_wave/3, stop/1]).

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
	  id,
	  peerid,
	  status				% incoming or outgoing
	 }).

start_link(Client, Cmd) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Client, Cmd, self()], []),
    {ok, Pid}.

answer(Call) ->
    gen_server:call(Call, answer).

drop(Call) ->
    drop(Call, "hangup").

drop(Call, Reason) ->
    gen_server:call(Call, {drop, Reason}).

play_wave(Call, Notify, WaveFile) ->
    error_logger:info_msg("play_wave ~p ~p ~p~n", [?MODULE, self(), Notify]),
    gen_server:call(Call, {play_wave, Notify, WaveFile, self()}).

stop(Call) ->
    gen_server:cast(Call, stop).

%%
%% gen_server callbacks
%%
init([Client, Cmd, Parent]) ->
    {ok, Handle} = yate:open(Client),
    Status =
	case command:find_key(status, Cmd) of
	    {ok, "incoming"} ->
		incoming;
	    {ok, "outgoing"} ->
		outgoing;
	    error ->
		outgoing
    end,

    State0 = #state{client=Client,parent=Parent,handle=Handle},
    State = setup(Status, Cmd, State0),
    Id = State#state.id,
    error_logger:info_msg("~p: call ~p ~p~n", [?MODULE, Id, Status]),
    {ok, State}.

setup(incoming, Cmd, State) ->
    Id = command:fetch_key(id, Cmd),
    Handle = State#state.handle,
    ok = yate:watch(Handle, call.execute,
		    fun(Cmd1) ->
			    Id == command:fetch_key(id, Cmd1)
		    end),
    State#state{peerid=Id,status=incoming};
setup(outgoing, Cmd, State) ->
    Id = command:fetch_key(id, Cmd),
    Peerid = command:fetch_key(peerid, Cmd),
    State1 = State#state{id=Id,peerid=Peerid,status=incoming},
    ok = setup_watches(State1),
    State1.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(answer, _From, State) ->
    Id = State#state.id,
    Peerid = State#state.peerid,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, call.answered},
		       {id, Id},
  		       {targetid, Peerid},
		       {module, "erlang"}
		      ]),
    {reply, ok, State};

handle_call({drop, Reason}, _From, State) ->
    Id = State#state.id,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, "call.drop"},
		       {id, Id},
		       {reason, Reason},
		       {module, "erlang"}
		      ]),
    {reply, ok, State};

handle_call({play_wave, Notify, WaveFile, Pid}, _From, State) ->
    Id = State#state.id,
    Handle = State#state.handle,
    {ok, NotifyPid} = yate_notify:start_link(State#state.client, Notify, Pid),
    {ok, NotifyId} = yate_notify:get_id(NotifyPid),
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, "chan.attach"},
		       {id, Id},
		       {notify, NotifyId},
		       {source, ["wave/play/", WaveFile]}
		      ]),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info({'EXIT', Pid, Reason}, State=#state{parent=Pid}) ->
    {stop, Reason, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#state.handle,
    yate:close(Handle),
    terminated.


handle_command(message, Dir, Cmd, From, State) ->
    Name = (Cmd#command.header)#message.name,
    handle_message(Name, Dir, Cmd, From, State).


handle_message(call.execute, ans, Cmd, _From, State) ->
    Peerid = command:fetch_key(peerid, Cmd),
    State1 = State#state{id=Peerid},
    ok = setup_watches(State1),
    Parent = State#state.parent,
    Parent ! {yate_call, execute, self()},
    {noreply, State1};
handle_message(chan.disconnected, ans, _Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, disconnected, self()},
    {noreply, State};
handle_message(chan.hangup, ans, _Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, hangup, self()},
    {stop, normal, State}.


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

