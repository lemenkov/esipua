%%%
%%% @doc       Demo call
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo_call).

-behaviour(gen_fsm).

%% api
-export([start_link/4, start/3]).

%% gen_fsm
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4,
	 init/1, terminate/3, route/2]).

-record(sstate, {handle, id}).

-include("yate.hrl").

-define(TIMEOUT_WAIT_EXEC, 10000). %% 10s

start(Client, Cmd, Args) ->
    Id = dict:fetch(id, Cmd#command.keys),
    start_link(Client, Id, Cmd, Args).

%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start demo server
%% @end
%%--------------------------------------------------------------------
start_link(Client, Id, Cmd, Args) ->
    error_logger:info_msg("yate_demo_call start_link~n"),
    gen_fsm:start_link(yate_demo_call, [Client, Id, Cmd, Args], []).

%% start(Client, Id, Cmd, Args) ->
%%     gen_fsm:start(yate_demo_call, [Client, Id, Cmd, Args], []).

%% gen_fsm
init([Client, Id, ExecCmd, _Args]) ->
    error_logger:info_msg("Init call ~p~n", [Id]),
    {ok, Handle} = yate:open(Client),
    ok = yate:watch(Handle, call.execute, 
		    fun(Cmd) ->
			    CmdId = dict:fetch(id, Cmd#command.keys),
			    Id == CmdId
		    end),
%%     ok = yate:install(Handle, chan.notify),

    NewKeys = dict:store(callto, "dumb/", ExecCmd#command.keys),
    NewCmd = ExecCmd#command{keys=NewKeys},
    yate:ret(Handle, NewCmd, false),

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


handle_info({yate, _Dir, Cmd, From}, StateName, StateData) ->
    Id = dict:fetch(id, Cmd#command.keys),
    handle_command(Cmd#command.type, Id, Cmd, From, StateName, StateData);
handle_info(Info, StateName, StateData) ->
    error_logger:error_msg("Unsupported info: ~p~n", [Info]),
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, StateData) ->
    Handle = StateData#sstate.handle,
    yate:unwatch(Handle, call.execute), 
    yate:unwatch(Handle, chan.hangup),
    yate:uninstall(Handle, chan.dtmf),
    yate:close(Handle),
    terminate.

code_change(_OldVsn, StateName, StateData, _Extra)  ->
    {ok, StateName, StateData}.


handle_command(message, Id, Cmd, From, StateName, StateData) ->
    Name = (Cmd#command.header)#message.name,
    handle_message(Name, Id, Cmd, From, StateName, StateData).


handle_message(call.execute, Id, Cmd, _From, route, StateData) ->
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
%%     ok = yate:watch(StateData#sstate.handle, chan.notify,
%% 		    fun(Cmd1) ->
%% 			    Id == dict:fetch(targetid, Cmd1#command.keys)
%% 		    end),
    ok = answer(Id, Cmd, StateData),
    ok = record_wave(Cmd, StateData),
    {next_state, execute, StateData};
handle_message(chan.dtmf, _Id, Cmd, _From, execute, StateData) ->
    Text = dict:fetch(text, Cmd#command.keys),
    handle_dtmf(Text, Cmd, execute, StateData);
handle_message(chan.hangup, _Id, _Cmd, _From, _State, StateData) ->
    error_logger:info_msg("Call hangup ~p~n", [StateData#sstate.id]),
    {stop, normal, StateData}.

handle_dtmf(Text, Cmd, execute, StateData) ->
    Handle = StateData#sstate.handle,
    error_logger:info_msg("Call dtmf ~p~n", [Text]),
    yate:ret(Handle, Cmd, true),
    {next_state, execute, StateData}.

answer(Id, Cmd, StateData) ->
    Handle = StateData#sstate.handle,
    {ok, _RetValue, _RetCmd} = yate:send_msg(Handle, call.answered, [{id, dict:fetch(targetid, Cmd#command.keys)}, {targetid, Id}, {module, "erlang"}]),
    ok.

record_wave(Cmd, StateData) ->
    Handle = StateData#sstate.handle,
    %%WaveFile = "/var/local/tmp/cvs/asterisk.net/sounds/digits/1.gsm",
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, "chan.attach"},
		       {id, dict:fetch(targetid, Cmd#command.keys)},
		       {notify, StateData#sstate.id},
		       %%{source, ["wave/play/", WaveFile]}
		       {maxlen, 8000},
		       {consumer, "wave/record//tmp/record.mulaw"}
		      ]),
    ok.
