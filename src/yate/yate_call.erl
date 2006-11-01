-module(yate_call).

-behaviour(gen_server).

-include("yate.hrl").

%% api
-export([
	 start_link/1,
	 start_link/2,
	 start_link/3,
%% 	 execute_link/2,
%% 	 execute_link/3,
	 execute/2,
	 answer/1, drop/2, drop/1,
	 play_wave/3, play_tone/2, start_rtp/2, start_rtp/3,
	 ringing/1, progress/1, send_dtmf/2, stop/1]).

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
	  rtpid,
	  localip,
	  status				% incoming or outgoing
	 }).

start_link(Client) when is_pid(Client) ->
    start_link(Client, self()).

start_link(Client, Owner) when is_pid(Client), is_pid(Owner) ->
    gen_server:start_link(?MODULE, [Client, Owner], []);

start_link(Client, Cmd) ->
    start_link(Client, Cmd, self()).

start_link(Client, Cmd, Owner) ->
    gen_server:start_link(?MODULE, [Client, Cmd, Owner], []).

%% execute_link(Client, Keys) ->
%%     execute_link(Client, Keys, self()).

%% execute_link(Client, Keys, Owner) ->
%%     gen_server:start_link(?MODULE, [Client, Keys, Owner], []).

execute(Call, Keys) ->
    gen_server:call(Call, {execute, Keys}).

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

start_rtp(Call, Remote_address) ->
    error_logger:info_msg("~p: start_rtp~n", [?MODULE]),
    gen_server:call(Call, {start_rtp, Remote_address}).

send_dtmf(Call, Dtmf) ->
    gen_server:call(Call, {send_dtmf, Dtmf}).

ringing(Call) ->
    gen_server:call(Call, ringing).

progress(Call) ->
    gen_server:call(Call, progress).

stop(Call) ->
    gen_server:cast(Call, stop).

%%
%% gen_server callbacks
%%
init([Client, Parent]) ->
    init_common(outgoing, Client, [], Parent);

init([Client, Cmd, Parent]) when is_record(Cmd, command) ->
    init_common(incoming, Client, [Cmd], Parent).

%% init([Client, Keys, Parent]) when is_list(Keys) ->
%%     init_common(outgoing, Client, [Keys], Parent).

init_common(Status, Client, Args, Parent) ->
    error_logger:info_msg("~p: ~p ~p~n", [?MODULE, self(), Status]),
    process_flag(trap_exit, true),
    link(Parent),
    {ok, Handle} = yate:open(Client),
    State0 = #state{client=Client,parent=Parent,handle=Handle},
    setup(Status, Args, State0).


setup(incoming, [Cmd], State) ->
    Id = command:fetch_key(id, Cmd),
    Handle = State#state.handle,
    ok = yate:watch(Handle, call.execute,
		    fun(Cmd1) ->
			    case command:find_key(module, Cmd1) of
				{ok, _Driver} ->
				    Id == command:fetch_key(id, Cmd1);
				_ ->
				    false
			    end
		    end),
    {ok, State#state{peerid=Id,status=incoming}};

setup(outgoing, [], State) ->
    {ok, State}.


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

handle_call({execute, Keys}, _From, State) ->
    Handle = State#state.handle,
    Parent = State#state.parent,
    %% TODO async msg
    {ok, RetValue, RetCmd} = yate:send_msg(Handle, call.execute, Keys),
    case RetValue of
	false ->
	    %% TODO return false
%% 	    Parent ! {yate_call, notfound, self()},
%% 	    {stop, normal, State};
	    %% TODO change to error state?
	    {reply, {error, {noroute, RetCmd}}, State};
	true ->
	    {ok, Auto} = fetch_auto_keys(RetCmd),
	    Id = command:fetch_key(id, RetCmd),
	    Peerid = command:fetch_key(peerid, RetCmd),
	    State1 = State#state{id=Id,peerid=Peerid,status=outgoing},
%% 	    {ok, State2} = setup(State1),
	    ok = setup_watches(State1),
	    Parent ! {yate_call, Auto, RetCmd, self()},
	    {reply, ok, State1}
    end;


handle_call(answer, _From, State) ->
    Id = State#state.id,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, call.answered},
		       {id, Id},
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

    {ok, RetValue, RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, chan.rtp},
		       {id, Id},
		       {transport, "RTP/AVP"},
		       {direction, bidir},
		       {media, audio},
		       {remoteip, Remote_address},
		       {remoteport, Remote_port},
		       {format, Format}
		      ]),
    
    {Reply, State1} =
	case RetValue of
	    true ->
		Localip =
		    case command:find_key(localip, RetCmd) of
			{ok, Localip1} ->
			    Localip1;
			error ->
			    State#state.localip
		    end,

		Localport =
		    case command:find_key(localport, RetCmd) of
			{ok, Localport1} ->
			    list_to_integer(Localport1);
			error ->
			    undefined
		    end,
		Rtpid =
		    case command:find_key(rtpid, RetCmd) of
			{ok, Rtpid1} ->
			    Rtpid1;
			error ->
			    undefined
		    end,
		State2 = State#state{rtpid=Rtpid, localip=Localip},
		{{ok, Localip, Localport}, State2};

	    false ->
		{{error, yate_error}, State}
	end,
    {reply, Reply, State1};

handle_call({start_rtp, Remote_address}, _From, State) ->
    Id = State#state.id,
    Handle = State#state.handle,
    Format = alaw,

    {ok, true, RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, chan.rtp},
		       {transport, "RTP/AVP"},
		       {media, "audio"},
		       {direction, "receive"},
		       {id, Id},
		       {format, Format},
		       {remoteip, Remote_address}
		      ]),

    Localip =
	case command:find_key(localip, RetCmd) of
	    {ok, Localip1} ->
		Localip1;
	    error ->
		State#state.localip
	end,
    Localport = list_to_integer(command:fetch_key(localport, RetCmd)),

    {reply, {ok, Localip, Localport}, State#state{localip=Localip}};

handle_call(ringing, _From, State) ->
    Id = State#state.id,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, "call.ringing"},
 		       {id, Id},
		       {module, "erlang"}
		      ]),
    
    {reply, ok, State};

handle_call(progress, _From, State) ->
    Id = State#state.id,
    Handle = State#state.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, "call.progress"},
 		       {id, Id},
		       {module, "erlang"},
		       %% FIXME enable only if available.
		       {earlymedia, true},
 		       {media, yes}
		      ]),
    
    {reply, ok, State};

handle_call({send_dtmf, Dtmf}, _From, State) ->
    Id = State#state.id,
    Rtpid = State#state.rtpid,
    Handle = State#state.handle,
    ok =
	yate:queue_msg(Handle, chan.masquerade,
		      [
		       {message, "chan.dtmf"},
 		       {id, Id},
		       {targetid, Rtpid},
		       {text, Dtmf}
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
    error_logger:error_msg("~p:received cmd ~p~n", [?MODULE, self()]),
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
    error_logger:error_msg("~p:received ~p ~p ~p~n", [?MODULE, self(), Name, Dir]),
    handle_message(Name, Dir, Cmd, From, State).


handle_message(call.execute, ans, Cmd, _From, State) ->
    Peerid = command:fetch_key(peerid, Cmd),
    State1 = State#state{id=Peerid},
    ok = setup_watches(State1),
    Parent = State#state.parent,
    Parent ! {yate_call, execute, self()},
    {noreply, State1};
handle_message(call.ringing, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, ringing, Cmd, self()},
    {noreply, State};
handle_message(call.progress, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, progress, Cmd, self()},
    {noreply, State};
handle_message(call.answered, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, answered, Cmd, self()},
    {noreply, State};
handle_message(chan.disconnected, ans, Cmd, _From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, disconnected, Cmd, self()},
    {noreply, State};
handle_message(chan.dtmf, req, Cmd, From, State) ->
    Parent = State#state.parent,
    Parent ! {yate_call, dtmf, Cmd, self()},
    ok = yate:ret(From, Cmd, true),
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
    ok = yate:install(Handle, chan.dtmf,
		      fun(Cmd) ->
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
