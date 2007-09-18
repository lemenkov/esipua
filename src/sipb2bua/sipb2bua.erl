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
-export([]).

-export([make/0]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,

	 initial/3
	]).

-record(state, {
		old_pid,
		invite,				% INVITE request
		incoming_call,			% SIP call (Pid)
		outgoing_call,			% SIP call (Pid)
		outgoing_inv,			% SIP call (Pid)
		contact				% Local contact
	       }).

-define(SERVER, ?MODULE).


%% yxa_app callbacks
-export([init/0, request/2,  response/2]).

%%-include("siprecords.hrl").
-include("siprecords.hrl").
-include("sipsocket.hrl").

init() ->
    #yxa_app_init{}.

request(#request{method="INVITE"}=Request, YxaCtx) when is_record(YxaCtx, yxa_ctx) ->
%%     Origin = YxaCtx#yxa_ctx.origin,
    LogStr = YxaCtx#yxa_ctx.logstr,

    {ok, Pid} = start_link(LogStr),
    ok = receive_invite(Pid, Request);
request(Request, YxaCtx) when is_record(YxaCtx, yxa_ctx) ->
    LogStr = YxaCtx#yxa_ctx.logstr,
    logger:log(normal, "sipb2bua: Request ~s", [LogStr]),
    siphelper:send_response(Request, 501 ,"Not Implemented"),
    ok.


response(Response, YxaCtx) when is_record(Response, response), is_record(YxaCtx, yxa_ctx) ->
    Origin = YxaCtx#yxa_ctx.origin,
    LogStr = YxaCtx#yxa_ctx.logstr,

    {Status, Reason} = {Response#response.status, Response#response.reason},
    if
	Status >= 200, Status =< 299 ->
	    case sipdialog:get_dialog_controller(Response) of
		{ok, Dc_pid} ->
		    logger:log(normal, "sipcall: Response to ~s: '~p ~s', no matching transaction, matching dialog ~p - dropping", [LogStr, Status, Reason, Dc_pid]);
		_ ->
%% 		    logger:log(normal, "sipb2bua: Response to ~s: '~p ~s', no matching transaction, no matching dialog - dropping X", [LogStr, Status, Reason]),
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
start_link(LogStr) ->
    logger:log(normal, "sipb2bua: start_link ~p~n", [self()]),
    Spec = {make_ref(),
	    {gen_fsm, start_link, [?MODULE, [LogStr, self()], []]},
	    temporary, 2000, worker, [sipb2bua]},
    supervisor:start_child(sipserver_sup, Spec).
%%     gen_fsm:start_link(?MODULE, [LogStr, self()], []).


receive_invite(Pid, Request) ->
    gen_fsm:sync_send_event(Pid, {receive_invite, Request}).

stop() ->
    error.

%%
%% gen_fsm callbacks
%%

%% sipcall
init([]) ->
    {ok, undefined};

%% Incoming SIP call
init([LogStr, OldPid]) ->
    process_flag(trap_exit, true),
    logger:log(normal, "sipb2bua: INVITE ~s ~p~n", [LogStr, self()]),
    {ok, initial, #state{old_pid=OldPid}}.


initial({receive_invite, Request}, _From, State) ->
    Callid = sipheader:callid(Request#request.header),
    {ok, SipCall} = sipcall:start(Callid),
    ok = sipcall:receive_invite(SipCall, Request, State#state.old_pid),

    Header = Request#request.header,
    Body = Request#request.body,
    Uri = Request#request.uri,

%%     [Target] = url_param:find(Uri#sipurl.param_pairs, "target"),
%%     TargetUri = sipurl:parse(Target),
    TargetUri = Uri,

    {FromName, FromUri} = sipheader:from(Header),
    FromContact = contact:new(FromName, FromUri, []),

%%     {_ToName, _ToUri} = sipheader:to(Header),
%%     ToContact = contact:new(ToName, ToUri, []),

%%     UriContact = contact:new(Uri),
    UriContact = contact:new(TargetUri),
    {ok, Outgoing_inv} = sipcall:build_invite(FromContact, UriContact, Body),

    IsHomedomain = local:homedomain(Uri),
    Out_header = Outgoing_inv#request.header,
    Out_header2 =
	if
	    IsHomedomain ->
		%% Add route to incoming proxy
		Out_header1 =
		    keylist:prepend({"Route", ["<sip:skinner.hem.za.org;lr>"]},
				    Out_header),
		logger:log(normal, "sipb2bua: header ~p ~p~n", [self(), Out_header1]),
		Out_header1;
	    true ->
		%% Send directly to target
		Out_header
	end,

%%	    ok = sipcall:drop(SipCall, Status, Reason),

    Out_header3 = keylist:set("User-Agent", ["YXA-sipb2bua/0.1"], Out_header2),

    Outgoing_inv1 = Outgoing_inv#request{header=Out_header3},
    
    OutCallid = sipheader:callid(Outgoing_inv1#request.header),
    {ok, Outgoing_call} = sipcall:start(OutCallid),
    ok = sipcall:send_invite(Outgoing_call, Outgoing_inv1),

    Contact = "<sip:dummy@192.168.0.2:5080>",

    State1 = State#state{invite=Request,
		   outgoing_call=Outgoing_call,
		   outgoing_inv=Outgoing_inv1,
		   incoming_call=SipCall,
		   contact=Contact
		  },
    {reply, ok, start, State1}.

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
    {Status1, Reason1} =
	case Status of
	    401 ->
		{503, "Service Unavailable"};
	    403 ->
		{503, "Service Unavailable"};
	    407 ->
		{503, "Service Unavailable"};
	    _ ->
		{Status, Reason}
	end,
 
    ok = sipcall:drop(Call, Status1, Reason1),
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

%% handle_info({'EXIT', Pid, normal=Reason}, StateName, State) ->
%%     %% Ignore normal exit
%%     error_logger:info_msg("~p: ~p terminated ~p~n",
%% 			  [?MODULE, Pid, Reason]),
%%     {next_state, StateName, State};

%% handle_info({'EXIT', Pid, Reason}, _StateName, State) ->
%%     %% Terminate with error
%%     error_logger:info_msg("~p: ~p terminated ~p~n",
%% 			  [?MODULE, Pid, Reason]),
%%     {next_state, StateName, State};
%% %%     {stop, Reason, State};

handle_info(Info, StateName, State) ->
    error_logger:error_msg("~p: Unhandled info in info=~p state_name=~p~n",
			   [?MODULE, Info, StateName]),
    {next_state, StateName, State}.


terminate(Reason, _StateName, _State) ->
    error_logger:error_msg("~p: Terminated ~p~n", [?MODULE, Reason]),
    terminated.
