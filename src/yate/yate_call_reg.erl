-module(yate_call_reg).

-behaviour(gen_server).

%% api
-export([
	 start_link/0,
	 stop/0,
	 register_call/2,
	 unregister_call/1,
	 get_call/1,
	 get_call/3,
	 execute_call/1
	]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  calls=[]
	 }).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

register_call(Id, Call) ->
    gen_server:call(?SERVER, {register_call, Id, Call}).

unregister_call(Call) ->
    gen_server:call(?SERVER, {unregister_call, Call}).

get_call(Id) ->
    gen_server:call(?SERVER, {get_call, Id}).

get_call(Client, Id, ExecCmd) ->
    gen_server:call(?SERVER, {get_call, Client, Id, ExecCmd, self()}).

execute_call(Client) ->
    gen_server:call(?SERVER, {execute_call, Client, self()}).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, #state{}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({get_call, Id}, _From, State) ->
    Calls = State#state.calls,
    case lists:keysearch(Id, 1, Calls) of
	{value, {_Id, Call}} ->
	    {reply, {ok, Call}, State};
	false ->
	    {reply, {error, not_found}, State}
    end;

handle_call({get_call, Client, Id, ExecCmd, Owner}, _From, State) ->
    Calls = State#state.calls,
    case lists:keysearch(Id, 1, Calls) of
	{value, {_Id, Call}} ->
	    {reply, {ok, Call}, State};
	false ->
	    {ok, Call} = yate_call:start_link(Client, ExecCmd, Owner),
	    Calls1 = [{Id, Call}|Calls],

	    %% TODO save user pid
	    {reply, {ok, Call}, State#state{calls=Calls1}}
    end;

handle_call({execute_call, Client, Owner}, _From, State) ->
    Calls = State#state.calls,
    {ok, Call} = yate_call:start_link(Client, Owner),
    Calls1 = [{outgoing, Call}|Calls],

    %% TODO save user pid
    {reply, {ok, Call}, State#state{calls=Calls1}};

handle_call({register_call, Id, Call}, _From, State) ->
    Calls = State#state.calls,
    case lists:keysearch(Id, 1, Calls) of
	{value, _} ->
	    {reply, {error, already_registered}, State};
	false ->
	    Calls1 = [{Id, Call}|Calls],
	    {reply, ok, State#state{calls=Calls1}}
    end;

handle_call({unregister_call, Call}, _From, State) ->
    Calls = State#state.calls,
    case lists:keysearch(Call, 2, Calls) of
	{value, _} ->
	    Calls1 = lists:keydelete(Call, 2, Calls),
	    {reply, ok, State#state{calls=Calls1}};
	false ->
	    {reply, {error, not_registered}, State}
    end;

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unsupported call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unsupported cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Info, State) ->
    error_logger:error_msg("Unsupported info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.
