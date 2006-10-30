%%
%% Based on sipclient.erl from Yxa
%%

%%
%% States:
%% start    - Start state
%% up       - Call up
%% bye_sent - SIP BYE sent
%%

-module(sipb2bua).

-behaviour(gen_fsm).


%% api
-export([start_link/2, stop/0]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3]).

-record(state, {
		invite,				% INVITE request
		incoming_call,			% SIP call (Pid)
		outgoing_call,			% SIP call (Pid)
		outgoing_inv,			% SIP call (Pid)
		contact				% Local contact
	       }).

-define(SERVER, ?MODULE).


%% yxa_app callbacks
-export([init/0, request/3, response/3]).

-include("siprecords.hrl").
-include("sipsocket.hrl").

init() ->
    Callregister = {callregister, {callregister, start_link, []},
		    permanent, 2000, worker, [callregister]},
    Tables = [],
    [Tables, stateful, {append, [Callregister]}].

request(#request{method="INVITE"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    sipb2bua:start_link(Request, LogStr);
request(_Request, _Origin, LogStr) ->
    logger:log(normal, "sipclient: Request ~s", [LogStr]),
    ok.


response(Response, Origin, LogStr) when is_record(Response, response), is_record
(Origin, siporigin) ->        
    {Status, Reason} = {Response#response.status, Response#response.reason},
    if
	Status >= 200, Status =< 299 ->
	    case sipdialog:get_dialog_controller(Response) of
		{ok, Dc_pid} ->
		    logger:log(normal, "sipcall: Response to ~s: '~p ~s', no matching transaction, matching dialog ~p - dropping", [LogStr, Status, Reason, Dc_pid]);
		_ ->
%% 		    logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction, no matching dialog - dropping X", [LogStr, Status, Reason]),
		    sipcall:response(Response, Origin, LogStr)
	    end;
	true ->
            logger:log(normal, "sipcall: Response to ~s: '~p ~s', no matching transaction - dropping",
		   [LogStr, Status, Reason])
    end,
    ok.

%%
%% outgoing yate call
%%
start_link(Request, LogStr) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    gen_fsm:start_link(?MODULE, [Request, LogStr, self()], []).


stop() ->
    error.

%%
%% gen_fsm callbacks
%%

%% sipcall
init([]) ->
    {ok, undefined};

%% Incoming SIP call
init([Request, LogStr, OldPid]) ->
    logger:log(normal, "sipclient: INVITE ~s ~p~n", [LogStr, self()]),
    {ok, SipCall} = sipcall:start_link(?MODULE, [], []),
    ok = sipcall:receive_invite(SipCall, Request, OldPid),

    Header = Request#request.header,
    Body = Request#request.body,
    Uri = Request#request.uri,

    {FromName, FromUri} = sipheader:from(Header),
    FromContact = contact:new(FromName, FromUri, []),

    {ToName, ToUri} = sipheader:to(Header),
%%     ToContact = contact:new(ToName, ToUri, []),

    UriContact = contact:new(Uri),

    {ok, Outgoing_inv} = sipcall:build_invite(FromContact, UriContact, Body),
    
    {ok, Outgoing_call} = sipcall:start_link(?MODULE, [], []),
    ok = sipcall:send_invite(Outgoing_call, Outgoing_inv),

    Contact = "<sip:dummy@192.168.0.2:5080>",

    State = #state{invite=Request,
		   outgoing_call=Outgoing_call,
		   outgoing_inv=Outgoing_inv,
		   incoming_call=SipCall,
		   contact=Contact
		  },
    {ok, start, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


handle_sync_event(Event, _From, StateName, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Event]),
    {reply, ok, StateName, State}.


handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(Request, StateName, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {next_state, StateName, State}.


handle_info({call_drop, SipCall, Response}, _StateName, #state{outgoing_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.incoming_call,
    Status = Response#response.status,
    Reason = Response#response.reason,
    ok = sipcall:drop(Call, Status, Reason),
    {stop, normal, State};

handle_info({call_drop, SipCall, Request}, _StateName, #state{incoming_call=SipCall}=State) when is_record(Request, request) ->
    Call = State#state.outgoing_call,
%%     Status = Response#response.status,
%%     Reason = Response#response.reason,
    ok = sipcall:drop(Call),
    {stop, normal, State};

handle_info({call_drop, SipCall, Request}, _StateName, #state{outgoing_call=SipCall}=State) when is_record(Request, request) ->
    Call = State#state.incoming_call,
%%     Status = Response#response.status,
%%     Reason = Response#response.reason,
    ok = sipcall:drop(Call),
    {stop, normal, State};

handle_info({call_drop, SipCall, ExtraHeaders}, _StateName, #state{incoming_call=SipCall}=State) when is_list(ExtraHeaders) ->
    Call = State#state.outgoing_call,
%%     Status = Response#response.status,
%%     Reason = sipstatus_to_reason(Status),
    ok = sipcall:drop(Call),
    {stop, normal, State};


handle_info({call_proceeding, SipCall, Response}, start=StateName, #state{outgoing_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.incoming_call,
    Status = Response#response.status,
    Reason = Response#response.reason,
    Body = Response#response.body,

    ok = sipcall:proceeding(Call, Status, Reason, Body),
    {next_state, StateName, State};

handle_info({call_redirect, SipCall, Response}, start=StateName, #state{outgoing_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.incoming_call,
    Status = Response#response.status,
    Reason = Response#response.reason,
    ok = sipcall:drop(Call, Status, Reason),
    {next_state, StateName, State};

handle_info({call_answered, SipCall, Response}, start=_StateName, #state{outgoing_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.incoming_call,
%%     Status = Response#response.status,
%%     Reason = Response#response.reason,
    Body = Response#response.body,

    ok = sipcall:answer(Call, Body),
    {next_state, up, State};

handle_info({'EXIT', _Pid, normal}, StateName, State) ->
    %% Ignore normal exit
    {next_state, StateName, State};

handle_info({'EXIT', _Pid, Reason}, _StateName, State) ->
    %% Terminate with error
    {stop, Reason, State};

handle_info(Info, StateName, State) ->
    error_logger:error_msg("~p: Unhandled info in info=~p state_name=~p~n",
			   [?MODULE, Info, StateName]),
    {next_state, StateName, State}.


terminate(Reason, _StateName, _State) ->
    error_logger:error_msg("~p: Terminated ~p~n", [?MODULE, Reason]),
    terminated.
