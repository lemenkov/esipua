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

-record(sstate, {client, handle, id, call}).

-include("yate.hrl").

-define(TIMEOUT_WAIT_EXEC, 10000). %% 10s

start(Client, Cmd, From, Args) ->
    Id = command:fetch_key(id, Cmd),
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
    {ok, Call} = yate_call:start_link(Client, ExecCmd),
    {ok, Handle} = yate:open(Client),

    NewCmd = command:append_keys([
				   {callto, "dumb/"},
				   {autoring, true}
				  ],
				  ExecCmd),
    yate:ret(From, NewCmd, false),

    {ok, route, #sstate{client=Client, handle=Handle, id=Id, call=Call}, ?TIMEOUT_WAIT_EXEC}.

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


handle_info({yate_notify, Tag}, StateName, StateData) ->
    error_logger:info_msg("Notify ~p~n", [Tag]),
    ok = yate_call:drop(StateData#sstate.call),
    {next_state, StateName, StateData};

handle_info({yate_call, execute, _From}, _StateName, StateData) ->
    error_logger:info_msg("Call execute ~p. answer~n", [?MODULE]),

    ok = yate_call:answer(StateData#sstate.call),
    ok = play_wave(StateData),
    {next_state, execute, StateData};

handle_info({yate_call, disconnected, _From}, _StateName, StateData) ->
    {next_state, disconnected, StateData};

handle_info({yate_call, hangup, _From}, _StateName, StateData) ->
    error_logger:info_msg("Call hangup ~p~n", [self()]),
    {stop, normal, StateData};

handle_info(Info, StateName, StateData) ->
    error_logger:error_msg("Unsupported info: ~p~n", [Info]),
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, StateData) ->
    Handle = StateData#sstate.handle,
    yate:close(Handle),
    terminate.

code_change(_OldVsn, StateName, StateData, _Extra)  ->
    {ok, StateName, StateData}.


%% handle_message(chan.dtmf, req, Cmd, From, execute, StateData) ->
%%     Text = command:fetch_key(text, Cmd),
%%     handle_dtmf(Text, Cmd, From, execute, StateData);
%% handle_dtmf(Text, Cmd, From, execute, StateData) ->
%%     error_logger:info_msg("Call dtmf ~p~n", [Text]),
%%     yate:ret(From, Cmd, true),
%%     {next_state, execute, StateData}.

record_wave(_Cmd, StateData) ->
%%     Handle = StateData#sstate.handle,
%%     TargetId = command:fetch_key(targetid, Cmd),
    Notify = StateData#sstate.id,
%%    play_wave(Handle, TargetId, Notify, "/var/local/tmp/cvs/asterisk.net/sounds/demo-thanks.gsm").
    yate_call:play_wave(StateData#sstate.call, Notify, "/var/local/tmp/cvs/asterisk.net/sounds/demo-congrats.gsm").
%%    play_wave(Handle, TargetId, Notify, "/tmp/record.mulaw").
%%    record_wave(Handle, TargetId, Notify, "/tmp/record.mulaw", 8000).

play_wave(StateData) ->
    Call = StateData#sstate.call,
    yate_call:play_wave(Call, StateData#sstate.id,
			"/var/local/tmp/cvs/asterisk.net/sounds/digits/0.gsm").


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
