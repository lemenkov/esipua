%%%
%%% @doc       TCP connection server
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_conn_srv).

-include("yate.hrl").

-behaviour(gen_server).

%% api
-export([start_link/3, stop/1]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

%% pending: Dictionary of {Type, Id} -> From
-record(sstate, {sock, pid, queue, pending=dict:new(), installed=dict:new(),
		 watched=dict:new()}).

-define(SERVER, ?MODULE).

start_link(Host, Port, Pid) ->
    gen_server:start_link(?MODULE, [Host, Port, Pid], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%
%% gen_server callbacks
%%
init([Host, Port, Pid]) ->
    init([Host, Port, Pid, []]);
init([Host, Port, Pid, Options]) ->
    NewOptions = Options ++ [list, {packet, line}],
    {ok, Sock} = gen_tcp:connect(Host, Port, NewOptions),
    Header = #connect{role=global,type=""},
    Cmd = #command{id=erl,header=Header,type=connect},
    ok = send_command(Sock, req, Cmd),
    {ok, #sstate{sock=Sock,pid=Pid}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({install, Name, Prio}, From, State) ->
    case dict:is_key(Name, State#sstate.installed) of
	false ->
	    Header = #install{priority=Prio},
	    Cmd = #command{id=Name,header=Header,type=install},
	    {ok, NewState} = queue_command(req, Cmd, {call, From}, State),
	    NewInstalled = dict:append(Name, Prio, State#sstate.installed),
	    {noreply, NewState#sstate{installed=NewInstalled}};
	true ->
	    {reply, {ok, true}, State}
    end;
handle_call({uninstall, Name}, From, State) ->
    Installed = State#sstate.installed,
    case dict:is_key(Name, Installed) of
	true ->
	    Cmd = #command{id=Name,type=uninstall},
	    {ok, NewState} = queue_command(req, Cmd, {call, From}, State),
	    NewInstalled = dict:erase(Name, Installed),
	    {noreply, NewState#sstate{installed = NewInstalled}};
	false ->
	    {reply, {ok, true}, State}
    end;
handle_call({watch, Name}, From, State) ->
    case dict:is_key(Name, State#sstate.watched) of
	false ->
	    Cmd = #command{id=Name,type=watch},
	    {ok, NewState} = queue_command(req, Cmd, {call, From}, State),
	    NewDict = dict:append(Name, From, State#sstate.watched),
	    {noreply, NewState#sstate{watched=NewDict}};
	true ->
	    {reply, {ok, true}, State}
    end;
handle_call({unwatch, Name}, From, State) ->
    Dict = State#sstate.watched,
    case dict:is_key(Name, Dict) of
	true ->
	    Cmd = #command{id=Name,type=unwatch},
	    {ok, NewState} = queue_command(req, Cmd, {call, From}, State),
	    NewDict = dict:erase(Name, Dict),
	    {noreply, NewState#sstate{watched = NewDict}};
	false ->
	    {reply, {ok, true}, State}
    end;
handle_call({msg, Name, Keys}, From, State) ->
    {ok, NewState} = queue_message(Name, Keys, {call, From}, State),
    {noreply, NewState};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({ret, Cmd}, State) ->
    Sock = State#sstate.sock,
    ok = send_command(Sock, ans, Cmd),
    {noreply, State};
handle_cast({msg, Name, Keys, Pid, Tag}, State) ->
    {ok, NewState} = queue_message(Name, Keys, {cast, Pid, Tag}, State),
    {noreply, NewState};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({tcp, _Socket, Data}, State) ->
    {ok, NewState} = handle_tcp(Data, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket, Reason}, State) ->
    error_logger:info_msg("TCP closed: ~p~n", [Reason]),
    {noreply, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    error_logger:info_msg("TCP error: ~p~n", [Reason]),
    {noreply, State};
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


queue_message(Name, Keys, Tag, State) ->
    Time = yate_util:seconds(),
    Header = #message{time=Time,name=Name},
    KeyDict = dict:from_list(Keys),
    Id = erlang:ref_to_list(make_ref()),
    Cmd = #command{id=Id,retvalue="",type=message,header=Header,keys=KeyDict},
    {ok, NewState} = queue_command(req, Cmd, Tag, State),
    {ok, NewState}.


queue_command(Dir, Cmd, Tag, State) ->
    ok = send_command(State#sstate.sock, Dir, Cmd),
    Key = {Cmd#command.type, Cmd#command.id},
    Pending = dict:store(Key, Tag, State#sstate.pending),
    {ok, State#sstate{pending=Pending}}.


handle_tcp(Data, State) ->
    {ok, Dir, Cmd} = yate_decode:decode_command(Data),
    {ok, NewState} = handle_command(Dir, Cmd, State),
    {ok, NewState}.

handle_command(ans, Cmd, State) ->
    Id = Cmd#command.id,
    Key = {Cmd#command.type, Id},
    case dict:find(Key, State#sstate.pending) of
	{ok, Tag} ->
	    Reply = Cmd#command.retvalue,
	    case Tag of
		{call, From} ->
		    gen_server:reply(From, {ok, Reply, Cmd});
		{cast, Pid, Tag2} ->
		    Pid ! {cast, {ans, Reply, Cmd}, Tag2};
		_ ->
		    error_logger:error_msg("handle_command wrong tag ~p~n",
					   [Tag])
	    end,
	    Pending = dict:erase(Key, State#sstate.pending),
	    {ok, State#sstate{pending=Pending}};
	error ->
	    Pid = State#sstate.pid,
	    Pid ! {yate, ans, Cmd, self()},
	    {ok, State}
    end;
handle_command(req, Cmd, State) when Cmd#command.type == message ->
    Pid = State#sstate.pid,
    Pid ! {yate, req, Cmd, self()},
    {ok, State};
handle_command(req, Cmd, State) ->
    error_logger:error_msg("Unhandled request in ~p: ~p~n", [?MODULE, Cmd]),
    {ok, State}.


send_command(Sock, Dir, Cmd) ->
    {ok, Msg} = yate_encode:encode_command(Cmd#command.type, Dir, Cmd),
    ok = gen_tcp:send(Sock, Msg).
