-module(yate_notify).

-include("yate.hrl").

-behaviour(gen_server).

%% api
-export([start_link/3, get_id/1, stop/0]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  client,
	  handle,
	  tag,
	  pid,
	  id
	 }).

-define(SERVER, ?MODULE).

start_link(Client, Tag, Pid) ->
    {ok, NotifyPid} = gen_server:start_link(?MODULE, [Client, Tag, Pid], []),
    {ok, NotifyPid}.

get_id(NotifyPid) ->
    gen_server:call(NotifyPid, get_id).

stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([Client, Tag, Pid]) ->
    {ok, Handle} = yate:open(Client),
    Id = "FIXME",
    ok = yate:install(Handle, chan.notify,
		    fun(Cmd) ->
 			    Id == command:fetch_key(targetid, Cmd)
		    end),
    {ok, #state{client=Client,handle=Handle,tag=Tag,pid=Pid,id=Id}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(get_id, _From, State) ->
    {reply, {ok, State#state.id}, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    yate:close(State#state.handle),
    terminated.

handle_command(message, Dir, Cmd, From, State) ->
    Name = (Cmd#command.header)#message.name,
    handle_message(Name, Dir, Cmd, From, State).

handle_message(chan.notify, req, Cmd, From, State) ->
    Pid = State#state.pid,
    Tag = State#state.tag,
    error_logger:error_msg("chan.notify in ~p ~p ~p~n", [?MODULE, Pid, Tag]),
    Pid ! {yate_notify, Tag},
    yate:ret(From, Cmd, true),
    {stop, normal, State}.
