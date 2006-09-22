%%%
%%% @doc       Yate client server
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_srv).

-include("yate.hrl").
-include("yate_srv.hrl").

-behaviour(gen_server).

%% api
-export([start_link/2, stop/1, connect/3]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

%% pending: Dictionary of {Type, Id} -> From
-record(sstate, {conn,
		 pids=dict:new(),      %% Pid -> [pidentry()]
		 installed=dict:new(), %% Name = string() -> [install_entry()]
		 watched=dict:new(),   %% Name = string() -> [install_entry()]
		 pending=dict:new(),   %% Id = string() -> [install_entry()]
		 outgoing=dict:new()}).

-record(pidentry, {type, name}).

-define(SERVER, ?MODULE).


%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start server and connect to Yate as external module
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port) ->
    error_logger:info_msg("Start ~p~n", [?MODULE]),
    gen_server:start_link(?MODULE, [Host, Port], []).


%%--------------------------------------------------------------------
%% @spec connect(Handle, Host, Port) -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Connect to Yate as external module
%% @end
%%--------------------------------------------------------------------
connect(Handle, Host, Port) ->
    gen_server:call(Handle, {connect, Host, Port}).


%%--------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc Stop server
%% @end
%%--------------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid, stop).


%%
%% gen_server callbacks
%%
init([Host, Port]) ->
    error_logger:info_msg("Start ~p ~p~n", [?MODULE, self()]),
    error_logger:info_msg("Connecting ~p~n", [?MODULE]),
    {ok, Conn} = yate_conn_srv:start_link(Host, Port, self()),
    error_logger:info_msg("Connected ~p~n", [?MODULE]),
%%    link(Conn),
%%     process_flag(trap_exit, true),
    {ok, #sstate{conn=Conn}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({connect, Host, Port}, _From, State) ->
    {ok, Conn} = yate_sup:start_conn(Host, Port, self()),
    {reply, ok, State#sstate{conn=Conn}};
handle_call({client, {install, Name, Fun}, Pid}, _From, State) ->
    Installed = State#sstate.installed,
    {ok, NewInstalled, NewPids} = install(install, Name, Pid, Fun, State, Installed),
    {reply, ok, State#sstate{installed=NewInstalled, pids=NewPids}};

handle_call({client, {uninstall, Name}, Pid}, _From, State) ->
    Installed = State#sstate.installed,
    {ok, NewInstalled, NewPids} = uninstall(install, Name, Pid, State, Installed),
    {reply, ok, State#sstate{installed=NewInstalled, pids=NewPids}};

handle_call({client, {watch, Name, Fun}, Pid}, _From, State) ->
    Installed = State#sstate.watched,
    {ok, NewInstalled, NewPids} = install(watch, Name, Pid, Fun, State, Installed),
    {reply, ok, State#sstate{watched=NewInstalled, pids=NewPids}};

handle_call({client, {unwatch, Name}, Pid}, _From, State) ->
    Installed = State#sstate.watched,
    {ok, NewInstalled, NewPids} = uninstall(watch, Name, Pid, State, Installed),
    {reply, ok, State#sstate{watched=NewInstalled, pids=NewPids}};

handle_call({client, {msg, Name, Keys, Tag}, Pid}, _From, State) ->
    ok = yate_conn:queue_msg(State#sstate.conn, Name, Keys, {queue, Pid, Tag}),
    {reply, ok, State};

handle_call({client, {msg, Name, Keys}, _Pid}, From, State) ->
    ok = yate_conn:queue_msg(State#sstate.conn, Name, Keys, {send, From}),
    {noreply, State};

handle_call({client, close, Pid}, From, State) ->
    {ok, State1} = uninstall_pid(Pid, State),
    unlink(Pid),
    {reply, ok, State1};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unsupported call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
%% handle_cast({cast, {ans, RetValue, RetCmd}, From}, State) ->
%%     error_logger:info_msg("Ans in ~p: ~p~n", [?MODULE, RetValue]),
%%     gen_server:reply(From, {ok, RetValue, RetCmd}),
%%     {noreply, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unsupported cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info({cast, {ans, RetValue, RetCmd}, {queue, Pid, Tag}}, State) ->
    Pid ! {cast, {ans, RetValue, RetCmd}, Tag},
    {noreply, State};
handle_info({cast, {ans, RetValue, RetCmd}, {send, From}}, State) ->
    gen_server:reply(From, {ok, RetValue, RetCmd}),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    case uninstall_pid(Pid, State) of
	{ok, State1} ->
	    error_logger:info_msg("~p: Client ~p exited ~p~n", [?MODULE, Pid, Reason]),
	    {noreply, State1};
	_ ->
	    exit(Reason)
    end;
handle_info(Info, State) ->
    error_logger:error_msg("Unsupported info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    error_logger:error_msg("~p terminated ~p~n", [?MODULE, Reason]),
    terminated.


handle_command(message, req, Cmd, From, State) ->
    handle_request((Cmd#command.header)#message.name, Cmd, From, State);
handle_command(message, ans, Cmd, From, State) ->
    handle_answer((Cmd#command.header)#message.name, Cmd, From, State);
handle_command(Type, _Dir, _Cmd, _From, State) ->
    error_logger:error_msg("Unsupported command in ~p: ~p~n", [?MODULE, Type]),
    {noreply, State}.

handle_request(Name, Cmd, _From, State) ->
    case dict:find(Name, State#sstate.installed) of
	{ok, EntryList} ->
%%	    send_pending(Cmd, EntryList, State);
	    Conn = State#sstate.conn,
	    yate_pending:start_link(Conn, Cmd, EntryList),
	    {noreply, State};
	error ->
%%	    yate_conn:ret(State#sstate.conn, Cmd, false),
	    error_logger:error_msg("Unhandled request in ~p: ~p~n", [?MODULE, Name]),
	    {noreply, State}
    end.

handle_answer(Name, Cmd, _From, State) ->
    case dict:find(Name, State#sstate.watched) of
	{ok, EntryList} ->
	    send_all(ans, Cmd, EntryList);
	error ->
	    error_logger:error_msg("Unhandled answer in ~p: ~p~n", [?MODULE, Name]),
	    ok
    end,
    {noreply, State}.


%% @doc Send Cmd to all entries at once
send_all(Type, Cmd, [Entry|R]) ->
    case (Entry#install_entry.func)(Cmd) of
	true ->
	    Entry#install_entry.pid ! {yate, Type, Cmd, self()};
	false ->
	    ok
    end,
    send_all(Type, Cmd, R);
send_all(_Type, _Cmd, []) ->
    ok.


install(Type, Name, Pid, Fun, State, Installed) ->
    case dict:is_key(Name, Installed) of
	false ->
	    ok = do_install(Type, State#sstate.conn, Name);
	true ->
	    ok
    end,
    InstallEntry = #install_entry{pid=Pid,func=Fun},
    NewInstalled = dict:append(Name, InstallEntry, Installed),
    PidEntry = #pidentry{type=Type, name=Name},
    NewPids = dict:append(Pid, PidEntry, State#sstate.pids),
    {ok, NewInstalled, NewPids}.


uninstall_pid(Pid, State) ->
    case dict:find(Pid, State#sstate.pids) of
	{ok, Entry} ->
	    {ok, State1} = uninstall_pid(Pid, Entry, State),
	    {ok, State1#sstate{pids=dict:erase(Pid, State1#sstate.pids)}};
	error ->
	    {ok, State}
    end.

uninstall_pid(Pid, [], State) ->
    {ok, State};
uninstall_pid(Pid, [PidEntry=#pidentry{type=watch}|R], State) ->
    {ok, Watched1, Pids1} = uninstall(watch, PidEntry#pidentry.name,
					Pid, State, State#sstate.watched),
    uninstall_pid(Pid, R, State#sstate{watched=Watched1});
uninstall_pid(Pid, [PidEntry=#pidentry{type=install}|R], State) ->
    {ok, Installed1, Pids1} = uninstall(install, PidEntry#pidentry.name,
					Pid, State, State#sstate.installed),
    uninstall_pid(Pid, R, State#sstate{installed=Installed1}).



uninstall(Type, Name, Pid, State, Installed) ->
    InstallList = dict:fetch(Name, Installed),
    NewInstallList = [InstallEntry || InstallEntry <- InstallList, InstallEntry#install_entry.pid =/= Pid],
    PidList = dict:fetch(Pid, State#sstate.pids),
    NewPidList = [PidEntry || PidEntry <- PidList, PidEntry#pidentry.name =/= Name],
    case NewInstallList of
	[] ->
	    NewInstalled = dict:erase(Name, Installed),
	    ok = do_uninstall(Type, State#sstate.conn, Name);
	_ ->
	    NewInstalled = dict:store(Name, NewInstallList, Installed)
    end,
    case NewPidList of
	[] ->
	    NewPids = dict:erase(Pid, State#sstate.pids);
	_ ->
	    NewPids = dict:store(Pid, NewPidList, State#sstate.pids)
    end,
    {ok, NewInstalled, NewPids}.


do_install(install, Handle, Name) ->
    yate_conn:install(Handle, Name);
do_install(watch, Handle, Name) ->
    yate_conn:watch(Handle, Name).

do_uninstall(install, Handle, Name) ->
    yate_conn:uninstall(Handle, Name);
do_uninstall(watch, Handle, Name) ->
    yate_conn:unwatch(Handle, Name).
