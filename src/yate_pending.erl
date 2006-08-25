%%
%% Send command sequencially to targets until one returns true
%% or all are tried
%%
-module(yate_pending).

-include("yate.hrl").
-include("yate_srv.hrl").

-behaviour(gen_server).

%% api
-export([start_link/3, stop/0]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {conn, cmd, entry_list}).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000).

start_link(Conn, Cmd, EntryList) ->
    gen_server:start_link(?MODULE, [Conn, Cmd, EntryList], []).

stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([Conn, Cmd, EntryList]) ->
    Header = Cmd#command.header,
    Name = Header#message.name,
    error_logger:info_msg("Start ~p ~p ~p ~p~n",
			  [?MODULE, self(), Name, EntryList]),
    State = #state{conn=Conn, cmd=Cmd, entry_list=EntryList},
    case send_pending(Cmd, State) of
	{ok, NewState} ->
	    {ok, NewState, ?TIMEOUT};
	{stop, Reason, _NewState} ->
	    error_logger:info_msg("Stop ~p ~p ~p~n", [?MODULE, self(), Name]),
	    {stop, Reason}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(_Request, _From, _State) ->
    exit(unhandled_call).


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, _State) ->
    exit(unhandled_cast).


handle_info({ret, Cmd}, State) ->
    Success = Cmd#command.success,
    handle_ret(Success, Cmd, State);
handle_info(timeout, State) ->
    handle_ret(false, State#state.cmd, State);
handle_info(_Info, _State) ->
    exit(unhandled_info).


terminate(_Reason, _State) ->
    terminated.


handle_ret(true, Cmd, State) ->
    error_logger:info_msg("Stop ~p~n", [?MODULE]),
    yate_conn:ret(State#state.conn, Cmd, true),
    {stop, normal, State};

handle_ret(false, Cmd, State) ->
    case send_pending(Cmd, State) of
	{ok, NewState} ->
	    {noreply, NewState, ?TIMEOUT};
	{stop, Reason, NewState} ->
	    {stop, Reason, NewState}
    end.


send_pending(Cmd, State) ->
    EntryList = State#state.entry_list,
    case send_once(req, Cmd, EntryList) of
	{ok, NewEntryList} ->
	    {ok, State#state{entry_list=NewEntryList}};
	error ->
	    yate_conn:ret(State#state.conn, Cmd, false),
	    {stop, normal, State}
    end.


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
    error.

