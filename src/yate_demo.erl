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

-record(sstate, {handle, client}).

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
		      fun(Cmd) ->
			      dict:fetch(called, Cmd#command.keys) == "99991009"
		      end),
    ok = yate:install(Handle, call.execute,
		      fun(_Cmd) ->
			      true
%% 			      Callto = dict:fetch(callto, Cmd#command.keys),
%% 			      Callto == "erl/test"
		      end),
    {ok, #sstate{handle=Handle, client=Client}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, _State) ->
    exit({unhandled, request, Request}).


handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:error_msg("EXIT: ~p ~p~n", [Pid, Reason]),
    {noreply, State}.


terminate(_Reason, State) ->
    Handle = State#sstate.handle,
    yate:uninstall(Handle, call.route),
    terminated.


handle_command(message, req, Cmd, From, State) ->
    handle_message((Cmd#command.header)#message.name, Cmd, From, State).


handle_message(call.execute, Cmd, From, State) ->
    Callto = dict:fetch(callto, Cmd#command.keys),
    error_logger:info_msg("Call execute ~p.~n", [Callto]),
    handle_call_execute(Callto, Cmd, From, State);
handle_message(call.route, Cmd, From, State) ->
    handle_call_route(dict:fetch(called, Cmd#command.keys), Cmd, From, State).


handle_call_execute("erl " ++ String, Cmd, _From, State) ->
    [File, FuncStr | Args] = string:tokens(String, " "),

    ModuleName = list_to_atom(File),
    Func = list_to_atom(FuncStr),

    %%IdStr = dict:fetch(id, Cmd#command.keys),
    %%Id = list_to_atom(IdStr),
%%     ChildSpec = {Id, {ModuleName, Func, [State#sstate.client, Cmd, Args]},
%% 		 temporary, 10, worker, [ModuleName]},
%%     yate_demo_sup:start_child(ChildSpec),
   apply(ModuleName, Func, [State#sstate.client, Cmd, Args]),
    {noreply, State}.


handle_call_route("99991009", Cmd, _From, State) ->
    Id = dict:fetch(id, Cmd#command.keys),
    {ok, _Pid} = yate_demo_call:start_link(State#sstate.client, Id),
    Handle = State#sstate.handle,
    yate:ret(Handle, Cmd, true, "dumb/"),
%%    yate:ret(From, Cmd, true, "tone/dial"),
    {noreply, State};
handle_call_route(Called, Cmd, _From, State) ->
    Handle = State#sstate.handle,
    yate:ret(Handle, Cmd, false),
    error_logger:error_msg("Unhandled call.route to: ~p~n", [Called]),
    {noreply, State}.
