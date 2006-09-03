%%%
%%% @doc       Demo call
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_clock).

-behaviour(gen_server).

%% api
-export([start_link/5, start/4]).

%% gen_fsm
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
	 init/1, terminate/2, route/2]).

%% Debug
-export([wave_year/1, wave_number/1, wave_tens/1, wave_ones/1]).

-record(sstate, {handle, id, peer_id, waves}).

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
    error_logger:info_msg("yate_clock start_link~n"),
    gen_server:start_link(yate_clock, [Client, Id, Cmd, From, Args], []).

%% gen_server
init([Client, Id, ExecCmd, From, _Args]) ->
    {{Year, Month, Day}, {Hour, Min, Secs}} = erlang:localtime(),
    Waves = wave_month(Month) ++ wave_day(Day) ++ wave_year(Year),
    error_logger:info_msg("Waves ~p~n", [Waves]),
    error_logger:info_msg("Init clock ~p~n", [Id]),

    {ok, Handle} = yate:open(Client),
    ok = yate:watch(Handle, call.execute, 
		    fun(Cmd) ->
			    CmdId = dict:fetch(id, Cmd#command.keys),
			    Id == CmdId
		    end),

    NewKeys =
	yate_util:dict_store_list([
				   {callto, "dumb/"},
				   {autoring, false}
				  ],
				  ExecCmd#command.keys),
    NewCmd = ExecCmd#command{keys=NewKeys},
    yate:ret(From, NewCmd, false),

    {ok, #sstate{handle=Handle, id=Id, waves=Waves}, ?TIMEOUT_WAIT_EXEC}.

route(timeout, State) ->
    {stop, error, State}.

handle_cast(_Request, _State) ->
    exit(unhandled_cast).

handle_call(_Request, _From, _State) ->
    exit(unhandled_call).


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info(Info, State) ->
    error_logger:error_msg("Unsupported info: ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#sstate.handle,
    yate:unwatch(Handle, call.execute), 
    yate:unwatch(Handle, chan.hangup),
    yate:uninstall(Handle, chan.dtmf),
    yate:uninstall(Handle, chan.notify),
    yate:close(Handle),
    terminate.

code_change(_OldVsn, State, _Extra)  ->
    {ok, State}.


handle_command(message, Dir, Cmd, From, State) ->
    Name = (Cmd#command.header)#message.name,
    handle_message(Name, Dir, Cmd, From, State).


handle_message(call.execute, ans, Cmd, _From, State) ->
    Id = dict:fetch(id, Cmd#command.keys),
    Peerid = dict:fetch(peerid, Cmd#command.keys),
    error_logger:info_msg("Call execute ~p. answer~n", [Peerid]),
    ok = yate:watch(State#sstate.handle, chan.hangup,
		    fun(Cmd1) ->
			    Peerid == dict:fetch(id, Cmd1#command.keys)
		    end),
    ok = yate:install(State#sstate.handle, chan.dtmf,
		    fun(Cmd1) ->
			    Peerid == dict:fetch(peerid, Cmd1#command.keys)
			    %%Peerid == dict:fetch(targetid, Cmd1#command.keys)
		    end),
    ok = yate:install(State#sstate.handle, chan.notify,
		    fun(Cmd1) ->
			    Id == dict:fetch(targetid, Cmd1#command.keys)
		    end),

    ok = answer(Id, Cmd, State),
    [Wave_file | R] = State#sstate.waves,
    TargetId = dict:fetch(targetid, Cmd#command.keys),
    ok = play_wave(TargetId, State, Wave_file),
    {noreply, State#sstate{peer_id=Peerid,waves=R}};
handle_message(chan.notify, req, Cmd, From, State) ->
    Id = dict:fetch(targetid, Cmd#command.keys),
    error_logger:info_msg("Notify ~p~n", [Id]),
    yate:ret(From, Cmd, true),
    handle_notify(State#sstate.waves, State);
handle_message(chan.hangup, ans, Cmd, _From, State) ->
    Id = dict:fetch(id, Cmd#command.keys),
    error_logger:info_msg("Call hangup ~p~n", [Id]),
    {stop, normal, State}.

handle_notify([], State) ->
    ok = drop(State),
    {stop, normal, State};
handle_notify([Wave_file | R], State) ->
    [Wave_file | R] = State#sstate.waves,
    ok = play_wave(State#sstate.peer_id, State, Wave_file),
    {noreply, State#sstate{waves=R}}.

play_wave(TargetId, State, Wave_file) ->
    play_wave(State#sstate.handle, TargetId, State#sstate.id,
	      Wave_file).

play_wave(Handle, TargetId, Notify, WaveFile) ->
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [{message, "chan.attach"},
		       {id, TargetId},
		       {notify, Notify},
		       {source, ["wave/play/", WaveFile]}
		      ]),
    ok.

answer(Id, Cmd, State) ->
    Handle = State#sstate.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, call.answered},
		       {id, dict:fetch(targetid, Cmd#command.keys)},
 		       {targetid, Id},
		       {module, "erlang"}
		      ]),
    ok.

drop(State) ->
    Handle = State#sstate.handle,
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, chan.masquerade,
		      [
		       {message, "call.drop"},
		       {id, State#sstate.peer_id},
		       {reason, "hangup"},
		       {module, "erlang"}
		      ]),
    ok.

wave_month(Month) ->
    [["/var/local/tmp/cvs/asterisk.net/sounds/digits/mon-",
     integer_to_list(Month - 1),
     ".gsm"]].

wave_day(Day) ->
    if
	Day == 31 ->
	    ["/var/local/tmp/cvs/asterisk.net/sounds/digits/h-30.gsm",
	     "/var/local/tmp/cvs/asterisk.net/sounds/digits/h-1.gsm"];
	Day == 30  ->
	    ["/var/local/tmp/cvs/asterisk.net/sounds/digits/h-30.gsm"];
	Day > 20 ->
	    [["/var/local/tmp/cvs/asterisk.net/sounds/digits/h-20.gsm"],
	     ["/var/local/tmp/cvs/asterisk.net/sounds/digits/h-",
	      integer_to_list(Day - 20),
	      ".gsm"]];
	Day == 20 ->
	    ["/var/local/tmp/cvs/asterisk.net/sounds/digits/h-20.gsm"];
	true  ->
	    [["/var/local/tmp/cvs/asterisk.net/sounds/digits/h-",
	     integer_to_list(Day),
	     ".gsm"]]
    end.

wave_year(Year) ->
    Hundred = trunc(Year / 100),
    Rest = Year - Hundred * 100,
    wave_number(Hundred) ++ ["/var/local/tmp/cvs/asterisk.net/sounds/digits/hundred.gsm"] ++ wave_number(Rest).

wave_number(Number) ->
    Tens = trunc(Number / 10),
    Ones = Number - Tens * 10,
    wave_tens(Tens) ++ wave_ones(Ones).

wave_tens(Tens) ->
    if
	Tens > 0 ->
	    [["/var/local/tmp/cvs/asterisk.net/sounds/digits/",
	      integer_to_list(Tens), "0.gsm"]];
	true ->
	    []
    end.
    
wave_ones(Ones) ->
    if
	Ones > 0 ->
	    [["/var/local/tmp/cvs/asterisk.net/sounds/digits/",
	      integer_to_list(Ones), ".gsm"]];
	true ->
	    []
    end.

