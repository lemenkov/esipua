%%%
%%% @doc       Yate client server
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_srv).

-include("yate.hrl").

-behaviour(gen_server).

%% api
-export([start_link/0, stop/1, connect/3]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

%% pending: Dictionary of {Type, Id} -> From
-record(sstate, {conn, pids=dict:new(), installed=dict:new(),
		 watched=dict:new(), pending=dict:new(),
		 outgoing=dict:new()}).

-record(pidentry, {type, name}).

-record(install_entry, {pid, func}).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {ok, Pid}.

connect(Handle, Host, Port) ->
    gen_server:call(Handle, {connect, Host, Port}).

stop(Pid) ->
    gen_server:cast(Pid, stop).


%%
%% gen_server callbacks
%%
init([]) ->
    {ok, #sstate{}}.


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

handle_call({client, {msg, Name, Keys}, _Pid}, From, State) ->
    ok = yate_conn:queue_msg(State#sstate.conn, Name, Keys, From),
    {noreply, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unsupported call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({client, {ret, Cmd}, _Pid}, State) ->
    Processed = (Cmd#command.header)#message.processed,
    yate_conn:ret(State#sstate.conn, Cmd, Processed),
    {noreply, State};
%% handle_cast({cast, {ans, RetValue, RetCmd}, From}, State) ->
%%     error_logger:info_msg("Ans in ~p: ~p~n", [?MODULE, RetValue]),
%%     gen_server:reply(From, {ok, RetValue, RetCmd}),
%%     {noreply, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unsupported cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info({cast, {ans, RetValue, RetCmd}, From}, State) ->
    gen_server:reply(From, {ok, RetValue, RetCmd}),
    {noreply, State};
handle_info(Info, State) ->
    error_logger:error_msg("Unsupported info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
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
	    Id = Cmd#command.id,
	    {ok, NewEntryList} = send_once(req, Cmd, EntryList),
	    Pending = dict:store(Id, NewEntryList, State#sstate.pending),
	    {noreply, State#sstate{pending=Pending}};
	error ->
	    {noreply, State}
    end.

handle_answer(Name, Cmd, _From, State) ->
    case dict:find(Name, State#sstate.watched) of
	{ok, EntryList} ->
	    send_all(ans, Cmd, EntryList);
	error ->
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


%% @doc Send Cmd to first entry
send_once(Type, Cmd, [Entry|R]) ->
    case (Entry#install_entry.func)(Cmd) of
	true ->
	    Entry#install_entry.pid ! {yate, Type, Cmd, self()},
	    {ok, R};
	false ->
	    send_once(Type, Cmd, R)
    end;
send_once(_Type, _Cmd, []) ->
    {ok, []}.


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

uninstall(Type, Name, Pid, State, Installed) ->
    InstallList = dict:fetch(Name, Installed),
    NewInstallList = [InstallEntry || InstallEntry <- InstallList, InstallEntry#install_entry.pid =/= Pid],
    PidList = dict:fetch(Pid, State#sstate.pids),
    NewPidList = [PidEntry || PidEntry <- PidList, PidEntry#pidentry.name =/= Name],
    error_logger:info_msg("uninstall ~p ~p ~p ~p~n", [Name, Pid, NewInstallList, NewPidList]),
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
