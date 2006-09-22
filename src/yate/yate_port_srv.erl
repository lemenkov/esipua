%%%
%%% @doc       Yate port controller server
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
%%% TODO: Add log output
%%%
-module(yate_port_srv).

-behaviour(gen_server).

%% api
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(sstate, {pid, port, state=startup, waiting=[]}).

-define(SERVER, ?MODULE).
%% -define(PROG, "./wrapper /usr/bin/yate -vvvvvvvvv").
-define(PROG, "../yate/wrapper /home/mikael/tmp/cvs/yate/build_linux ./run -vvvvvvvvv").
-define(TIMEOUT_5S, 5000).


%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    error_logger:info_msg("Start ~p~n", [?MODULE]),
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ok = wait_running(),
%%gen_server:call(Pid, wait_running),
    {ok, Pid}.

wait_running() ->
    gen_server:call(?SERVER, wait_running).


%%--------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc Stop server
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([]) ->
    Port = erlang:open_port({spawn, ?PROG},
			    [exit_status, stream,
			     stderr_to_stdout, {line, 80}]),
    {ok, _TRef} = timer:send_after(?TIMEOUT_5S, timeout),
    {ok, #sstate{port=Port}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(wait_running, From, State) ->
    case State#sstate.state of
	running ->
	    {reply, ok, State};
	_ ->
	    Waiting = State#sstate.waiting,
	    {noreply, State#sstate{waiting=[From|Waiting]}}
    end;
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({Port, Request}, State) when Port == State#sstate.port ->
    handle_port(Request, State);
handle_info(timeout, State) ->
    case State#sstate.state of
	running ->
	    %% Ignore when running
	    {noreply, State};
	_ ->
	    {stop, timeout, State}
    end;
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.

handle_port({data, Data}, State) ->
%%    error_logger:info_msg("Data ~p~n", [Data]),
    {ok, NewState} = handle_data(Data, State),
%%     Logbuf = NewState#sstate.logbuf,
%%     {noreply, NewState#sstate{logbuf=Logbuf ++ [Text]}, 500};
    {noreply, NewState};
handle_port({exit_status, ExitStatus}, State) ->
    error_logger:info_msg("Yate exit!~p~n", [ExitStatus]),
    {stop, {exit_status, ExitStatus}, State};
handle_port(Info, State) ->
    error_logger:error_msg("Unhandled port message in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


handle_data({eol, String}, State) when State#sstate.state == startup ->
    {match, Start, Len} = regexp:first_match(String, "\\([0-9]+\\)"),
    PidStr = string:substr(String, Start + 1, Len - 2),
    Pid = list_to_integer(PidStr),
    error_logger:info_msg("Yate pid ~p~n", [Pid]),
    {ok, State#sstate{pid=Pid,state=starting}};
handle_data({eol, "Yate engine is initialized and starting up"}, State) ->
    error_logger:info_msg("Yate ready!~n"),
    Fun = fun(From) -> gen_server:reply(From, ok) end,
    lists:map(Fun, State#sstate.waiting),
    {ok, State#sstate{state=running,waiting=[]}};
handle_data({eol, Text}, State) ->
    %% skip text
    io:format("~s~n", [Text]),
    {ok, State};
handle_data({noeol, Text}, State) ->
    %% skip text
    io:format("~s", [Text]),
    {ok, State}.


terminate(Reason, State) ->
    error_logger:info_msg("Terminating in ~p ~p~n", [?MODULE, Reason]),
    kill(State#sstate.pid),
    terminated.

kill(Pid) when is_integer(Pid) ->
    os:cmd("kill " ++ integer_to_list(Pid)),
    ok;
kill(undefined) ->
    error.
