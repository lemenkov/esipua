%%%
%%% @doc       Demo call
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo_call).

-behaviour(gen_fsm).

%% api
-export([start_link/2]).

%% gen_fsm
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4,
	 init/1, terminate/3]).

-record(sstate, {handle, id}).

-include("yate.hrl").

start_link(Handle, Id) ->
    gen_fsm:start_link(yate_demo_call, [Handle, Id], []).


%% gen_fsm
init([Handle, Id]) ->
    error_logger:info_msg("Init call ~p~n", [Id]),
    ok = yate:watch(Handle, call.execute, 
		    fun(Cmd) ->
			    CmdId = dict:fetch(id, Cmd#command.keys),
			    Id == CmdId
		    end),
    ok = yate:watch(Handle, chan.hangup,
		    fun(Cmd) ->
			    CmdId = dict:fetch(id, Cmd#command.keys),
			    Id == CmdId
		    end),
    ok = yate:install(Handle, chan.dtmf,
		    fun(Cmd) ->
			    CmdId = dict:fetch(id, Cmd#command.keys),
			    Id == CmdId
		    end),
%%     ok = yate:install(Handle, chan.notify),
    {ok, route, #sstate{handle=Handle, id=Id}}.

%% Async
%% stateName(Event, StateData) ->
%%     {next_state, NextStateName, NewStateData, Timeout}.

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
    Id = case dict:find(id, Cmd#command.keys) of
	     {ok, MyId} ->
		 MyId;
	     error ->
		 ""
	 end,
    handle_command(Cmd#command.type, Id, Cmd, From, StateName, StateData);
handle_info(Info, StateName, StateData) ->
    error_logger:error_msg("Unsupported info: ~p~n", [Info]),
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, StateData) ->
    Handle = StateData#sstate.handle,
    yate:unwatch(Handle, call.execute), 
    yate:unwatch(Handle, chan.hangup),
    yate:uninstall(Handle, chan.dtmf),
    terminate.

code_change(_OldVsn, StateName, StateData, _Extra)  ->
    {ok, StateName, StateData}.


handle_command(message, Id, Cmd, From, StateName, StateData) ->
    handle_message((Cmd#command.header)#message.name, Id, Cmd, From, StateName, StateData);
handle_command(Type, _Id, _Cmd, _From, StateName, StateData) ->
    error_logger:error_msg("Unsupported command: ~p~n", [Type]),
    {next_state, StateName, StateData}.


handle_message(call.execute, Id, Cmd, _From, route, StateData) when Id == StateData#sstate.id ->
    error_logger:info_msg("Call execute ~p. answer~n", [Id]),
    ok = answer(Id, Cmd, StateData),
    ok = record_wave(Cmd, StateData),
    {next_state, execute, StateData};
handle_message(chan.dtmf, Id, Cmd, _From, execute, StateData) when Id == StateData#sstate.id ->
    Text = dict:fetch(text, Cmd#command.keys),
    handle_dtmf(Text, Cmd, execute, StateData);
handle_message(chan.hangup, Id, _Cmd, _From, _State, StateData) when Id == StateData#sstate.id ->
    error_logger:info_msg("Call hangup ~p~n", [StateData#sstate.id]),
    {stop, normal, StateData};
handle_message(Type, _Id, Cmd, _From, StateName, StateData) ->
    error_logger:error_msg("Unsupported message in ~p: ~p~n", [?MODULE, Type]),
    yate:ret(StateData#sstate.handle, Cmd, false),
    {next_state, StateName, StateData}.

handle_dtmf(Text, Cmd, execute, StateData) ->
    error_logger:info_msg("Call dtmf ~p~n", [Text]),
    yate:ret(StateData#sstate.handle, Cmd, true),
    {next_state, execute, StateData}.

answer(Id, Cmd, StateData) ->
    Handle = StateData#sstate.handle,
    {ok, _RetValue, _RetCmd} = yate:send_msg(Handle, call.answered, [{id, dict:fetch(targetid, Cmd#command.keys)}, {targetid, Id}]),
    ok.

record_wave(Cmd, StateData) ->
    Handle = StateData#sstate.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, "chan.attach"},
		       {id, dict:fetch(targetid, Cmd#command.keys)},
		       {notify, StateData#sstate.id},
		       %%{source, "wave/play//var/local/tmp/cvs/asterisk.net/sounds/digits/1.gsm"}
		       {maxlen, 8000},
		       {consumer, "wave/record//tmp/record.mulaw"}
		      ]),
    ok.
