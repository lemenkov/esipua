-module(yate_call).

-behaviour(gen_server).

-include("yate.hrl").

%% api
-export([start_link/2, execute_link/2, answer/1, drop/2, drop/1,
	 play_wave/3, play_tone/2, start_rtp/3,
	 ringing/1, stop/1]).

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
    %% Can't use gen_server:start_link, since we need to trap exit
    %% from our parent.
    {ok, Pid} = gen_server:start(?MODULE, [Client, Cmd, self()], []),
    link(Pid),
    {ok, Pid}.

execute_link(Client, Keys) ->
    %% Can't use gen_server:start_link, since we need to trap exit
    %% from our parent.
    {ok, Pid} = gen_server:start(?MODULE, [Client, Keys, self()], []),
    link(Pid),
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

play_tone(Call, Tone) ->
    error_logger:info_msg("play_tone ~p ~p ~p~n", [?MODULE, self(), Tone]),
    gen_server:call(Call, {play_tone, Tone}).

start_rtp(Call, Remote_address, Remote_port) ->
    error_logger:info_msg("~p: start_rtp~n", [?MODULE]),
    gen_server:call(Call, {start_rtp, Remote_address, Remote_port}).

ringing(Call) ->
    gen_server:call(Call, ringing).

stop(Call) ->
    gen_server:cast(Call, stop).

%%
%% gen_server callbacks
%%
init([Client, Cmd, Parent]) when is_record(Cmd, command) ->
    process_flag(trap_exit, true),
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
    {ok, State} = setup(Status, Cmd, State0),
    {ok, State};

init([Client, Keys, Parent]) when is_list(Keys) ->
    process_flag(trap_exit, true),
    {ok, Handle} = yate:open(Client),
    Status = outgoing,
    State0 = #state{client=Client,parent=Parent,handle=Handle},
    {ok, State} = setup(Status, Keys, State0),
    {ok, State}.

setup(incoming, Cmd, State) ->
    Id = command:fetch_key(id, Cmd),
    Handle = State#state.handle,
    ok = yate:watch(Handle, call.execute,
		    fun(Cmd1) ->
			    Id == command:fetch_key(id, Cmd1)
		    end),
    {ok, State#state{peerid=Id,status=incoming}};

setup(outgoing, Keys, State) ->
    Handle = State#state.handle,
    Parent = State#state.parent,
    {ok, RetValue, RetCmd} = yate:send_msg(Handle, call.execute, Keys),
    case RetValue of
	false ->
	    %% TODO return false
	    Parent ! {yate_call, notfound, self()},
	    ignore;
	true ->
	    {ok, Auto} = fetch_auto_keys(RetCmd),
	    Id = command:fetch_key(id, RetCmd),
	    Peerid = command:fetch_key(peerid, RetCmd),
	    State1 = State#state{id=Id,peerid=Peerid,status=outgoing},
%% 	    {ok, State2} = setup(State1),
	    Parent ! {yate_call, Auto, self()},
	    {ok, State1}
    end.


fetch_auto_keys(Cmd) ->	    
    Autokeys = [answered, ringing, progress],
    case fetch_auto_keys(Cmd, Autokeys, []) of
	{ok, noauto} ->
	    case command:find_key(targetid, Cmd) of
		error ->
		    {ok, answered};
		_ ->
		    {ok, dialog}
	    end;
	{ok, Auto} ->
	    Auto
    end.

fetch_auto_keys(_Cmd, [], _Res) ->
    {ok, noauto};
fetch_auto_keys(Cmd, [Key|R], Res) ->
    case command:find_key(Key, Cmd) of
	{ok, "true"} ->
	    {ok, Key};
	_ ->
	    fetch_auto_keys(Cmd, R, Res)
    end.


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
    {ok, State1} = handle_drop(Reason, State),
    {reply, ok, State1};

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

handle_call({play_tone, Tone}, _From, State) ->
    Id = State#state.id,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, "chan.attach"},
		       {id, Id},
		       {source, "tone/" ++ Tone}]),
    {reply, ok, State};

%% handle_call({record_wave, Handle, TargetId, Notify, WaveFile, MaxLen) ->
%%     {ok, _RetValue, _RetCmd} =
%% 	yate:send_msg(Handle, chan.masquerade,
%% 		      [{message, "chan.attach"},
%% 		       {id, TargetId},
%% 		       {notify, Notify},
%% 		       {maxlen, MaxLen},
%% 		       {consumer, ["wave/record/", WaveFile]}
%% 		      ]),
%%     ok.

handle_call({start_rtp, Remote_address, Remote_port}, _From, State) ->
    Id = State#state.id,
    Handle = State#state.handle,
    Format = alaw,

    {ok, _RetValue, RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, chan.attach},
		       {id, Id},
		       {notify, tag},
		       {source, "rtp/*"},
 		       {consumer, "rtp/*"},
		       {remoteip, Remote_address},
		       {remoteport, Remote_port},
		       {format, Format}
		      ]),

    Localip = command:fetch_key(localip, RetCmd),
    Localport = list_to_integer(command:fetch_key(localport, RetCmd)),

%%     case command:find_key(localport, RetCmd) of
%% 	{ok, Local_port} ->
%% 	    logger:log(normal, "sipclient: local rtp port ~p", [Local_port]);
%% 	_ ->
%% 	    ok
%%     end,

    {reply, {ok, Localip, Localport}, State};

handle_call(ringing, _From, State) ->
    Id = State#state.id,
    Peerid = State#state.peerid,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, "call.ringing"},
 		       {id, Id},
   		       {targetid, Peerid},
		       {module, "erlang"}
		      ]),
    
    {reply, ok, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    error_logger:error_msg("~p:stop received ~p~n", [?MODULE, self()]),
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info({'EXIT', Pid, Reason}, State=#state{parent=Pid}) ->
    error_logger:error_msg("Do drop ~p ~p~n", [?MODULE, Reason]),
    {ok, State1} = handle_drop("Error", State),
    {noreply, State1};
handle_info({'EXIT', _Pid, normal}, State) ->
    %% Ignore normal exit
    {noreply, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    error_logger:error_msg("~p:EXIT received ~p~n", [?MODULE, self()]),
    {stop, Reason, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(Reason, State) ->
    error_logger:error_msg("~p: Terminating ~p~n", [?MODULE, Reason]),
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
handle_message(chan.disconnected, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, disconnected, Cmd, self()},
    {noreply, State};
handle_message(chan.hangup, ans, _Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, hangup, self()},
    error_logger:error_msg("~p:hangup received ~p~n", [?MODULE, self()]),
    {stop, normal, State}.


handle_drop(Reason, State) ->
    error_logger:info_msg("~p:handle_drop ~p ~p~n", [?MODULE, Reason, State]),
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

%% startup(State, Id) ->
%%     {ok, _RetValue, RetCmd} =
%% 	yate:send_msg(State#state.handle, chan.masquerade,
%% 		      [
%% 		       {message, "chan.startup"},
%% 		       {id, Id},
%% 		       {driver, "erlang"}
%% 		      ]),
%%     {ok, State}.
