%%%
%%% @doc       Demo call
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo_call).

-behaviour(gen_fsm).

%% api
-export([start_link/5, start/4]).

%% gen_fsm
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4,
	 init/1, terminate/3, route/2]).

-record(sstate, {handle, id, peer_id}).

-include("yate.hrl").

-define(TIMEOUT_WAIT_EXEC, 10000). %% 10s

start(Client, Cmd, From, Args) ->
    Id = dict:fetch(id, Cmd#command.keys),
    start_link(Client, Id, Cmd, From, Args).

%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start demo server
%% @end
%%--------------------------------------------------------------------
start_link(Client, Id, Cmd, From, Args) ->
    error_logger:info_msg("yate_demo_call start_link~n"),
    gen_fsm:start_link(yate_demo_call, [Client, Id, Cmd, From, Args], []).

%% start(Client, Id, Cmd, Args) ->
%%     gen_fsm:start(yate_demo_call, [Client, Id, Cmd, Args], []).

%% gen_fsm
init([Client, Id, ExecCmd, From, _Args]) ->
    error_logger:info_msg("Init call ~p~n", [Id]),
    {ok, Handle} = yate:open(Client),
    ok = yate:watch(Handle, call.execute, 
		    fun(Cmd) ->
			    CmdId = dict:fetch(id, Cmd#command.keys),
			    Id == CmdId
		    end),
%%     ok = yate:install(Handle, chan.notify),

%%     Handle = State#sstate.handle,
    NewKeys = dict:store(callto, "dumb/", ExecCmd#command.keys),
    NewCmd = ExecCmd#command{keys=NewKeys},
    yate:ret(From, NewCmd, false),

    {ok, route, #sstate{handle=Handle, id=Id}, ?TIMEOUT_WAIT_EXEC}.

%% Async
%% stateName(Event, StateData) ->
%%     {next_state, NextStateName, NewStateData, Timeout}.

route(timeout, StateData) ->
    {stop, error, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% Sync
%% stateName(Event, From, StateData) ->
%%     {next_state, NextStateName, NewStateData, Timeout},
%%     {reply, Reply, NextStateName, NewStateData, Timeout}.

handle_sync_event(_Event, _From, StateName, StateData) ->
%    {next_state, NextStateName, NewStateData, Timeout}.
    {reply, {error, unknown_event}, StateName, StateData}.


handle_info({yate, Dir, Cmd, From}, StateName, StateData) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, StateName, StateData);
handle_info(Info, StateName, StateData) ->
    error_logger:error_msg("Unsupported info: ~p~n", [Info]),
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, StateData) ->
    Handle = StateData#sstate.handle,
    yate:unwatch(Handle, call.execute), 
    yate:unwatch(Handle, chan.hangup),
    yate:uninstall(Handle, chan.dtmf),
    yate:uninstall(Handle, chan.notify),
    yate:close(Handle),
    terminate.

code_change(_OldVsn, StateName, StateData, _Extra)  ->
    {ok, StateName, StateData}.


handle_command(message, Dir, Cmd, From, StateName, StateData) ->
    Name = (Cmd#command.header)#message.name,
    handle_message(Name, Dir, Cmd, From, StateName, StateData).


handle_message(call.execute, ans, Cmd, _From, route, StateData) ->
    Id = dict:fetch(id, Cmd#command.keys),
    Peerid = dict:fetch(peerid, Cmd#command.keys),
    error_logger:info_msg("Call execute ~p. answer~n", [Peerid]),
    ok = yate:watch(StateData#sstate.handle, chan.hangup,
		    fun(Cmd1) ->
			    Peerid == dict:fetch(id, Cmd1#command.keys)
		    end),
    ok = yate:install(StateData#sstate.handle, chan.dtmf,
		    fun(Cmd1) ->
			    Peerid == dict:fetch(peerid, Cmd1#command.keys)
			    %%Peerid == dict:fetch(targetid, Cmd1#command.keys)
		    end),
    ok = yate:install(StateData#sstate.handle, chan.notify,
		    fun(Cmd1) ->
			    Id == dict:fetch(targetid, Cmd1#command.keys)
		    end),

    ok = answer(Id, Cmd, StateData),
    ok = play_wave(Cmd, StateData),
    {next_state, execute, StateData#sstate{peer_id=Peerid}};
handle_message(chan.dtmf, req, Cmd, From, execute, StateData) ->
    Text = dict:fetch(text, Cmd#command.keys),
    handle_dtmf(Text, Cmd, From, execute, StateData);
handle_message(chan.notify, req, Cmd, From, execute, StateData) ->
    Id = dict:fetch(targetid, Cmd#command.keys),
%%     Handle = StateData#sstate.handle,
    error_logger:info_msg("Notify ~p~n", [Id]),
    yate:ret(From, Cmd, true),
    ok = drop(StateData),
    {stop, normal, StateData};
handle_message(chan.hangup, ans, Cmd, _From, _State, StateData) ->
    Id = dict:fetch(id, Cmd#command.keys),
    error_logger:info_msg("Call hangup ~p~n", [Id]),
    {stop, normal, StateData}.
%%    {stop, hangup, StateData}.

handle_dtmf(Text, Cmd, From, execute, StateData) ->
    error_logger:info_msg("Call dtmf ~p~n", [Text]),
    yate:ret(From, Cmd, true),
    {next_state, execute, StateData}.

record_wave(Cmd, StateData) ->
    Handle = StateData#sstate.handle,
    TargetId = dict:fetch(targetid, Cmd#command.keys),
    Notify = StateData#sstate.id,
%%    play_wave(Handle, TargetId, Notify, "/var/local/tmp/cvs/asterisk.net/sounds/demo-thanks.gsm").
    play_wave(Handle, TargetId, Notify, "/var/local/tmp/cvs/asterisk.net/sounds/demo-congrats.gsm").
%%    play_wave(Handle, TargetId, Notify, "/tmp/record.mulaw").
%%    record_wave(Handle, TargetId, Notify, "/tmp/record.mulaw", 8000).

play_tone(Cmd, StateData) ->
    TargetId = dict:fetch(targetid, Cmd#command.keys),
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(StateData#sstate.handle, chan.masquerade,
		      [{message, "chan.attach"},
		       {id, TargetId},
		       {notify, StateData#sstate.id},
		       {source, "tone/dial"}]),
    ok.

play_wave(Cmd, StateData) ->
    TargetId = dict:fetch(targetid, Cmd#command.keys),
    play_wave(StateData#sstate.handle, TargetId, StateData#sstate.id,
	      "/var/local/tmp/cvs/asterisk.net/sounds/digits/0.gsm").

record_wave(Handle, TargetId, Notify, WaveFile, MaxLen) ->
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, "chan.attach"},
		       {id, TargetId},
		       {notify, Notify},
		       {maxlen, MaxLen},
		       {consumer, ["wave/record/", WaveFile]}
		      ]),
    ok.

play_wave(Handle, TargetId, Notify, WaveFile) ->
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, "chan.attach"},
		       {id, TargetId},
		       {notify, Notify},
		       {source, ["wave/play/", WaveFile]}
		      ]),
    ok.

%% answer(Id, Cmd, StateData) ->
%%     Handle = StateData#sstate.handle,
%%     {ok, _RetValue, _RetCmd} =
%% 	yate:send_msg(Handle,
%% 		      call.answered,
%% 		      [
%% %%		       {id, dict:fetch(targetid, Cmd#command.keys)},
%%  		       {targetid, Id},
%% 		       {module, "erlang"}
%% 		      ]),
%%     ok.

answer(Id, Cmd, StateData) ->
    Handle = StateData#sstate.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, call.answered},
		       {id, dict:fetch(targetid, Cmd#command.keys)},
 		       {targetid, Id},
		       {module, "erlang"}
		      ]),
    ok.

drop(StateData) ->
    Handle = StateData#sstate.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, "call.drop"},
		       {id, StateData#sstate.peer_id},
		       {reason, "hangup"},
		       {module, "erlang"}
%% 		       {answered, "true"}
		      ]),
    ok.

%%     Handle = StateData#sstate.handle,
%%     case  yate:send_msg(Handle, call.route,
%% 					 [{called, "99991001"}]) of
%% 	{ok, true, RetCmd} ->
%% 	    Msg = RetCmd#command.header,
%% 	    Retvalue = Msg#message.retvalue,
%% 	    error_logger:info_msg("Route success ~p~n", [Retvalue]),
%% 	    {ok, Ret2, RetCmd2} = yate:send_msg(Handle, chan.masquerade,
%% 						[{message, "call.execute"},
%% 						 {id, Peerid},
%% 						 {callto, Retvalue}]),
%% 	    error_logger:info_msg("Execute returns ~p~n", [Ret2]),
%% 	    case Ret2 of
%% 		true ->
%% 		    Msg2 = RetCmd2#command.header,
%% 		    Retvalue2 = Msg2#message.retvalue,
%% 		    error_logger:info_msg("Execute ~p ~p~n", [Ret2, Retvalue2]);
%% 		_ ->
%% 		    ok
%% 	    end;
%% 	{ok, false, _RetCmd} ->
%% 	    error_logger:info_msg("Route failed~n")
%%     end,
