%%%
%%% @doc       Demo
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_demo).

-behaviour(gen_server).

-include("yate.hrl").

%% api
-export([start_link/0, stop/0, run/0]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {handle, client}).

-define(SERVER, ?MODULE).
-define(HOST, localhost).
-define(PORT, 15062).


%%--------------------------------------------------------------------
%% @spec run() -> ok
%% @doc run demo
%% @end
%%--------------------------------------------------------------------
run() ->
    yate_sup:start_link(),
    {ok, _Pid} = start_link(),
    ok.

    
%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | ignore | {error, Error}
%%           Pid = pid()
%%           Error = {already_started, Pid} | term()
%% @doc create demo process
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, 5000}]).


%%--------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc stop demo process
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([]) ->
    error_logger:info_msg("start ~p~n", [?MODULE]),
    {ok, Client} = yate:connect(?HOST, ?PORT),
    {ok, Handle} = yate:open(Client),
    ok = yate:install(Handle, call.route, 
		      fun(_Cmd) ->
			      true
		      end),
    ok = yate:install(Handle, call.execute,
		      fun(_Cmd) ->
			      true
		      end),
    {ok, #state{handle=Handle, client=Client}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, _State) ->
    exit({unhandled, request, Request}).


handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info({cast, {ans, RetValue, RetCmd}, {call, From}}, State) ->
    Id = command:fetch_key(id, RetCmd),
    error_logger:info_msg("Result ~p ~p~n", [RetValue, Id]),
    gen_server:reply(From, RetValue),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:error_msg("EXIT: ~p ~p~n", [Pid, Reason]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#state.handle,
    yate:uninstall(Handle, call.route),
    yate:uninstall(Handle, call.execute),
    yate:close(Handle),
    terminated.


handle_command(message, req, Cmd, From, State) ->
    handle_message((Cmd#command.header)#message.name, Cmd, From, State);
handle_command(message, ans, _Cmd, _From, State) ->
    error_logger:info_msg("Ignore answer/watch.~n", []),
    {noreply, State}.


handle_message(call.execute, Cmd, From, State) ->
    Callto = command:fetch_key(callto, Cmd),
    handle_call_execute(Callto, Cmd, From, State);
handle_message(call.route, Cmd, From, State) ->
    handle_call_route(command:fetch_key(called, Cmd), Cmd, From, State).


handle_call_execute("erl/" ++ String, Cmd, From, State) ->
    [File, FuncStr | Args] = string:tokens(String, "/"),

    ModuleName = list_to_atom(File),
    Func = list_to_atom(FuncStr),
    error_logger:info_msg("Handle Call execute ~p ~p ~p.~n", [ModuleName, Func, Args]),
    case catch apply(ModuleName, Func,
		     [State#state.client, Cmd, From, Args]) of
	{'EXIT', Term} ->
	    error_logger:error_msg("call.execute failed ~p.~n", [Term]),
	    yate:ret(From, Cmd, false);
	_ ->
	    ok
    end,
    {noreply, State};
handle_call_execute(_Called, Cmd, From, State) ->
    yate:ret(From, Cmd, false),
    {noreply, State}.


handle_call_route("demo", Cmd, From, State) ->
    yate:ret(From, Cmd, true, "erl/yate_demo_call/start"),
    {noreply, State};
handle_call_route("clock", Cmd, From, State) ->
    yate:ret(From, Cmd, true, "erl/yate_clock/start"),
    {noreply, State};
handle_call_route("echo2", Cmd, From, State) ->
    yate:ret(From, Cmd, true, "erl/sipclient/call/sip:600@mulder"),
    {noreply, State};
handle_call_route("mulder3;" ++ Exten, Cmd, From, State) ->
    yate:ret(From, Cmd, true, "erl/sipclient/call/sip:" ++ Exten ++ "@mulder"),
    {noreply, State};
handle_call_route("mikael", Cmd, From, State) ->
    error_logger:info_msg("Route mikael~n"),
    yate:ret(From, Cmd, true, "sip/sip:1002@mulder"),
    {noreply, State};
handle_call_route("3002", Cmd, From, State) ->
    yate:ret(From, Cmd, true, "erl/sipclient/call/sip:2002@skinner.hem.za.org"),
    {noreply, State};
handle_call_route("dial", Cmd, From, State) ->
    yate:ret(From, Cmd, true, "erl/sipclient/call/sip:99991001@192.168.0.7:5080"),
    {noreply, State};
handle_call_route("ydial", Cmd, From, State) ->
    yate:ret(From, Cmd, true, "sip/sip:99991001@192.168.0.7:5072"),
    {noreply, State};
handle_call_route("reason=" ++ ReasonStr, Cmd, From, State) ->
    yate:ret(From, Cmd, true, "reason/" ++ ReasonStr),
    {noreply, State};
handle_call_route(Called, Cmd, From, State) ->
    yate:ret(From, Cmd, false),
    error_logger:error_msg("Unhandled call.route to: ~p~n", [Called]),
    {noreply, State}.
