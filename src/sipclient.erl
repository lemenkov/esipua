%%
%% Based on sipclient.erl from Yxa
%%

-module(sipclient).

-behaviour(gen_fsm).


%% api
-export([start_link/3, stop/0, call/4]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3]).

-record(state, {dialog,				% SIP Dialog
		invite,				% In/out INVITE request
		invite_pid,			% In/out INVITE pid
		invite_branch,			% Outgoing INVITE branch
		invite_cseqno,			% Outgoing cseq
		bye_branch,			% BYE branch id
		bye_pid,			% BYE pid
		client,				% Yate client
		handle,				% Yate handle
		call,				% Yate call
		address,			% Remote RTP address
		port,				% Remote RTP port
		contact,			% Local contact
		sdp_body			% Local sdp_body
	       }).

-define(SERVER, ?MODULE).


-export([init/0, request/3, response/3]).
-export([test/0, start_generate_request/5]).
%%-export([call/1]).

-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("yate.hrl").
-include("sdp.hrl").

-define(DEFAULT_TIMEOUT, 50).
-define(HOST, "localhost").
-define(PORT, 15062).

init() ->
    Server = {ysip_srv, {ysip_srv, start_link, [?HOST, ?PORT]},
	      permanent, 2000, worker, [ysip_srv]},
    Tables = [],
    [Tables, stateful, {append, [Server]}].

request(#request{method="OPTIONS"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    logger:log(normal, "sipclient: Options ~s", [LogStr]),
    send_response(Request, 200, "Ok");
request(#request{method="INVITE"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    ysip_srv:invite(Request, LogStr);
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
		    logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction, matching dialog ~p - dropping", [LogStr, Status, Reason, Dc_pid]);
		_ ->
		    logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction, no matching dialog - dropping", [LogStr, Status, Reason])
	    end;
	true ->
            logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction - dropping",
		   [LogStr, Status, Reason])
    end,
    ok.

%%
%% outgoing yate call
%%
call(Client, Cmd, From, Args) when is_record(Cmd, command) ->
    Id = command:fetch_key(id, Cmd),
    start_link(Client, Id, Cmd, From, Args).

test() ->
    From = #contact{display_name = none,
		    urlstr = "sip:referer@skinner:5080",
		    contact_param = contact_param:to_norm([])
		   },
    To = #contact{display_name = none,
		    urlstr = "sip:1002@mulder",
		    contact_param = contact_param:to_norm([])
		   },
    Method = "OPTIONS",
    SDP = [],
    Contact = "sip:contact@localhost",
    {ok, Request, _CallId, _FromTag} =
	start_generate_request(Method,
                               From,
                               To,
                               [{"Contact", [Contact]},
                                {"Content-Type", ["application/sdp"]}
                               ],
                               list_to_binary(SDP)
                              ),

    {ok, _Pid, _Branch} = send_request(Request),

    %%ok = sipdialog:register_dialog_controller(CallId, FromTag, self()),
    ok.

%%--------------------------------------------------------------------
%% Function: generate_new_request(Method, State, Contact)
%%           Method = string(), SIP method
%%           State  = state record()
%% Descrip.: Generate a request template using values from the dialog
%%           state in State#state.dialog, or from the INVITE request
%%           created during startup and stored in
%%           State#state.invite_request (note that the INVITE request
%%           is always created, even though it is not always sent).
%% Returns : {ok, Request, NewDialog}
%%--------------------------------------------------------------------

generate_new_request(Method, Dialog, Contact) ->
    {ok, CSeqNum, Dialog1} = sipdialog:get_next_local_cseq(Dialog),
    generate_new_request(Method, Dialog1, Contact, CSeqNum).

generate_new_request(Method, Dialog, Contact, CSeqNum) ->
    %% Figure out a bunch of parameters in ways that vary depending on if we have
    [C] = contact:parse([Dialog#dialog.remote_target]),
    Route = case Dialog#dialog.route_set of
		[] ->
		    [];
		Route1 ->
		    [{"Route", Route1}]
	    end,
    To = contact:new(none, Dialog#dialog.remote_uri,
		[{"tag", Dialog#dialog.remote_tag}]),
    logger:log(normal, "Remote URI: ~p", [To]),
    TargetURI = sipurl:parse(C#contact.urlstr),
    From = contact:new(none, Dialog#dialog.local_uri,
			      [{"tag", Dialog#dialog.local_tag}]),
    Header = keylist:from_list([{"From",        [contact:print(From)]},
				{"To",          [contact:print(To)]},
                                {"Call-Id",     [Dialog#dialog.callid]},
                                {"CSeq",        [lists:concat([CSeqNum, " ", Method])]},
				{"Contact",     [Contact]}
                               |Route]),
    Request1 = #request{method = Method,
                        uri    = TargetURI,
                        header = Header
                       },
    Request = siprequest:set_request_body(Request1, <<>>),
    {ok, Request, Dialog}.




%%--------------------------------------------------------------------
%% Function: start_generate_request(Method, Referer, Referee, ExtraHeaders, Body)
%%           Method       = string(), SIP method
%%           Referer      = contact record()
%%           Referee      = contact record()
%%           ExtraHeaders = list() of {Key, ValueList} tuple()
%%           Body         = binary(), request body
%% Descrip.: Part of the startup functions. Build our initial request
%%           record.
%% Returns : {ok, Request, CallId, FromTag, CSeqNo}
%%           Request = request record()
%%           CallId  = string(), Call-Id of generated request
%%           FromTag = string(), From-tag of generated request
%%--------------------------------------------------------------------
start_generate_request(Method, From, To, ExtraHeaders, Body) ->
    FromTag = siputil:generate_tag(),
    {Megasec, Sec, Microsec} = now(),

    CallId = lists:concat([Megasec * 1000000 + Sec, "-", Microsec,
			   "@", siprequest:myhostname()
			  ]),
    CSeq = 1,

    FromContact = contact:add_param(From, "tag", FromTag),
    Header = keylist:from_list([{"From",	[contact:print(FromContact)]},
				{"To",		[contact:print(To)]},
				{"Call-Id",	[CallId]},
				{"CSeq",	[lists:concat([CSeq, " ", Method])]}
			       ] ++ ExtraHeaders),

    URI = sipurl:parse(To#contact.urlstr),
    Request1 = #request{method = Method, uri = URI, header = Header},
    Request = siprequest:set_request_body(Request1, Body),

    {ok, Request, CallId, FromTag, CSeq}.




start_link(Client, Request, LogStr) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    gen_fsm:start_link(?MODULE, [Client, Request, LogStr, self()], []).


start_link(Client, Id, Cmd, From, Args) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    gen_fsm:start_link(?MODULE, [Client, Id, Cmd, From, Args], []).


adopt_transaction(THandler, Pid) ->
    logger:log(normal, "sipclient: before change_parent ~p~n", [self()]),
    ok = transactionlayer:change_transaction_parent(THandler, self(), Pid),
    logger:log(normal, "sipclient: after change_parent ~p~n", [self()]),
    ok.


stop() ->
    error.

%%
%% gen_fsm callbacks
%%
init([Client, Request, LogStr, OldPid]) ->
    case transactionlayer:adopt_st_and_get_branchbase(Request) of
	ignore ->
	    {stop, {error, ignore}};
	error ->
	    {stop, error};
	BranchBase ->
	    init2(Client, Request, LogStr, BranchBase, OldPid)
    end;

init([Client, _Id, Cmd, From, [SipUri]]) ->
    {ok, Call} = yate_call:start_link(Client, Cmd),
    {ok, Handle} = yate:open(Client),

    NewCmd = command:append_keys([
				  {callto, "dumb/"},
				  {autoring, false}
				 ],
				 Cmd),
    yate:ret(From, NewCmd, false),

    Caller = case command:find_key(caller, Cmd) of
		 {ok, Caller1} ->
		     Caller1;
		 error ->
		     "anonymous"
	     end,
    CallerName = case command:find_key(callername, Cmd) of
		     {ok, CallerName1} ->
			 CallerName1;
		     error ->
			 none
		 end,

    CallerUri = "sip:" ++ Caller ++ "@192.168.0.7:5080",

    FromHdr = #contact{display_name = CallerName,
		       urlstr = CallerUri,
		       contact_param = contact_param:to_norm([])
		      },
    ToHdr = #contact{display_name = none,
		     urlstr = SipUri,
		     contact_param = contact_param:to_norm([])
		    },
    Method = "INVITE",

    Localip = "192.168.0.7",
    Localport = 12345,
    {ok, Body} = create_sdp_body(Localip, Localport),

    Contact = CallerUri,
    {ok, Request, _CallId, _FromTag, CSeqNo} =
	start_generate_request(Method,
                               FromHdr,
                               ToHdr,
                               [{"Contact", [Contact]},
                                {"Content-Type", ["application/sdp"]}
                               ],
			       Body
                              ),

    {ok, Pid, Branch} = send_request(Request),

    State = #state{client=Client,handle=Handle,call=Call,contact=Contact,
		   invite=Request,invite_pid=Pid,invite_branch=Branch,
		   sdp_body=Body, invite_cseqno=CSeqNo},
    {ok, outgoing, State}.

init2(Client, Request, LogStr, _BranchBase, OldPid) ->
    {ok, Handle} = yate:open(Client),
    logger:log(normal, "sipclient: INVITE ~s ~p~n", [LogStr, self()]),
    {ok, Address, Port} = parse_sdp(Request),
    THandler = transactionlayer:get_handler_for_request(Request),
    ok = transactionlayer:change_transaction_parent(THandler, OldPid, self()),
    Invite_pid = transactionlayer:get_pid_from_handler(THandler),
    State = #state{invite=Request, invite_pid=Invite_pid,
		   handle=Handle, address=Address, port=Port,
		   client=Client},
%%     {ok, _TRef} = timer:send_after(20000, timeout),
    execute(State).

parse_sdp(Request) ->
    Body = binary_to_list(Request#request.body),
    {ok, Sdp} = sdp:parse(Body),
    [Media|_] = Sdp#sdp.media,
    Conn = case Media#sdp_media.connection of
	undefined ->
	    Sdp#sdp.connection;
	Conn1 ->
	    Conn1
    end,
    Address = Conn#sdp_connection.address,
    Port = Media#sdp_media.port,
    {ok, Address, Port}.

execute(State) ->
    Call_to = "dumb/",
    Request = State#state.invite,
    Uri = Request#request.uri,
    Target = Uri#sipurl.user,
    From = keylist:fetch('from', Request#request.header),
    [FromContact] = contact:parse(From),
    FromUri = sipurl:parse(FromContact#contact.urlstr),
    Caller = FromUri#sipurl.user,
    Caller_name = FromContact#contact.display_name,
    {ok, Call} =
	yate_call:execute_link(State#state.client,
			       [
				{caller, Caller},
				{callername, Caller_name},
				{callto, Call_to},
				{target, Target}
			       ]),

    ok = send_response(State, 101, "Dialog Establishment"),
    State1 = State#state{call=Call},
    {ok, State2} = setup(State1),
    {ok, incoming, State2}.


setup(State) ->   
    Request = State#state.invite,

    %% FIXME Contact
    Contact = "<sip:dummy@192.168.0.7:5080>",
    {ok, Dialog} = create_dialog(Request, Contact),

%%     {ok, State1b} = startup(State, Id),
    {ok, State#state{contact=Contact,dialog=Dialog}}.


get_sdp_body(State) ->
    case State#state.sdp_body of
	undefined ->
	    {ok, State1} = start_rtp(State),
	    {ok, State1, State1#state.sdp_body};
	_ ->
	    {ok, State, State#state.sdp_body}
    end.

start_rtp(State) ->
    Call = State#state.call,
    Remote_address = State#state.address,
    Remote_port = State#state.port,
    {ok, Localip, Localport} =
	yate_call:start_rtp(Call, Remote_address, Remote_port),

    {ok, Body} = create_sdp_body(Localip, Localport),
    {ok, State#state{sdp_body=Body}}.

create_sdp_body(Localip, Localport) ->
    Seconds = integer_to_list(yate_util:seconds()),
    Origin = #sdp_origin{username="-", session_id=Seconds, version=Seconds,
			 network_type='IN', address_type='IP4',
			 address=Localip},
    Connection = #sdp_connection{network_type='IN', address_type='IP4',
				 address = Localip},
    Media = #sdp_media{media=audio, port=Localport, transport="RTP/AVP",
		       fmts=[8], connection=Connection},
    Sdp = #sdp{origin=Origin, session_name="Yxa", media=[Media]},
    Body = list_to_binary(lists:flatten(sdp:print(Sdp))),
    {ok, Body}.

create_dialog(Request, Contact) ->
    THandler = transactionlayer:get_handler_for_request(Request),
    {ok, ToTag} = transactionlayer:get_my_to_tag(THandler),
    {ok, Dialog} = sipdialog:create_dialog_state_uas(Request, ToTag, Contact),
    ok = sipdialog:register_dialog_controller(Dialog, self()),
    {ok, Dialog}.

%% TODO move 200ok to separate process and retransmitt
send_200ok(State) ->
    {ok, State1, Body} = get_sdp_body(State),
    ok = send_response(State1, 200, "Ok", [], Body),
    {ok, State1}.

send_response(Request, Status, Reason) ->
    send_response(Request, Status, Reason, []).

send_response(Request, Status, Reason, ExtraHeaders) ->
    send_response(Request, Status, Reason, ExtraHeaders, <<>>).

send_response(State, Status, Reason, ExtraHeaders, Body) when is_record(State, state) ->
    Request = State#state.invite,
    Contact = State#state.contact,
    send_response(Request, Status, Reason, ExtraHeaders, Body, Contact);
send_response(Request, Status, Reason, ExtraHeaders, Body) when is_record(Request, request) ->
    transactionlayer:send_response_request(Request, Status, Reason, ExtraHeaders, Body).


send_response(Request, Status, Reason, ExtraHeaders, Body, undefined) when is_record(Request, request) ->
    send_response(Request, Status, Reason, ExtraHeaders, Body);
send_response(Request, Status, Reason, ExtraHeaders, Body, _Contact) when is_record(Request, request), Status > 299 ->
    send_response(Request, Status, Reason, ExtraHeaders, Body);
send_response(Request, Status, Reason, ExtraHeaders, Body, Contact) when is_record(Request, request), Status =< 299 ->
    ExtraHeaders1 = [{"Contact", [Contact]}] ++ ExtraHeaders,
    send_response(Request, Status, Reason, ExtraHeaders1, Body).


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

handle_info({yate_call, dialog, Call}, StateName=incoming, State=#state{call=Call}) ->
    ok = send_response(State, 101, "Dialog Establishment"),
    {next_state, StateName, State};

handle_info({yate_call, ringing, Call}, StateName=incoming, State=#state{call=Call}) ->
    %% FIXME send sdp if earlymedia=true
    ok = send_response(State, 180, "Ringing"),
    {next_state, StateName, State};

handle_info({yate_call, progress, Call}, StateName=incoming, State=#state{call=Call}) ->
    {ok, State1, Body} = get_sdp_body(State),
    ok = send_response(State1, 183, "Session Progress", [], Body),
    {next_state, StateName, State1};

handle_info({yate_call, answered, Call}, StateName=incoming, State=#state{call=Call}) ->
    error_logger:info_msg("~p: autoanswer ~n", [?MODULE]),
%%     ok = yate_call:answer(Call),
    {ok, State1} = send_200ok(State),
    {next_state, StateName, State1};

handle_info({yate_call, disconnected, Call}, incoming=_StateName, State=#state{call=Call}) ->
    YReason = "Unknown",
%% 	case command:find_key(reason, Cmd) of
%% 	    {ok, YReason1} ->
%% 		YReason1;
%% 	    _ ->
%% 		none
%% 	end,
    error_logger:info_msg("~p: Call disconnect ~p~n", [?MODULE, YReason]),
    %% TODO distinguish CANCEL and BYE
    %% Send bye
%%     {ok, Bye, NewDialog} = generate_new_request("BYE", State#state.dialog,
%% 					       State#state.contact),
%%     {ok, Pid, Branch} = send_request(Bye),
%%     {next_state, State#state{dialog=NewDialog,bye_pid=Pid,bye_branch=Branch}};
    {Status, Reason} =
	case YReason of
	    "noroute" ->
		{404, "Not Found"};
	    "busy" ->
		{486, "Busy Here"};
 	    "forbidden" ->
 		{403, "Forbidden"};
	    _ ->
		{500, "Internal Server Error"}
	end,

    ok = send_response(State#state.invite, Status, Reason),
    {stop, normal, State};

handle_info({yate_call, disconnected, Call}, outgoing=_StateName, State=#state{call=Call}) ->
    %% TODO cancel INVITE
    Invite_pid = State#state.invite_pid,
    ExtraHeaders = [],
    Invite_pid ! {cancel, "hangup", ExtraHeaders},
    {stop, normal, State};

handle_info({yate_call, disconnected, Call}, up=StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: Call disconnected ~p ~p~n", [?MODULE, Call, StateName]),
    %% Send bye
    {ok, Bye, NewDialog} = generate_new_request("BYE", State#state.dialog,
					       State#state.contact),
    {ok, Pid, Branch} = send_request(Bye),
    State1 = State#state{dialog=NewDialog,bye_pid=Pid,bye_branch=Branch},
    {next_state, bye_sent, State1};

handle_info({yate_call, hangup, Call}, StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: Call hangup ~p ~p~n", [?MODULE, Call, StateName]),
    {next_state, StateName, State};

handle_info({servertransaction_cancelled, Pid, _ExtraHeaders}, incoming=StateName, #state{invite_pid=Pid}=State) ->
    %% FIXME
    logger:log(normal, "servertransaction_cancelled ~n", []),
    ok = send_response(State#state.invite, 487, "Request Terminated"),
    ok = yate_call:drop(State#state.call, "Cancelled"),
    {next_state, StateName, State};
handle_info({servertransaction_terminating, Pid}, incoming=StateName, #state{invite_pid=Pid}=State) ->
    %% FIXME
    logger:log(normal, "servertransaction_terminating ~n", []),
    {next_state, StateName, State};
handle_info(timeout, incoming=StateName, State) ->
    ok = send_response(State#state.invite, 408, "Request Timeout"),
    ok = yate_call:drop(State#state.call, "Request Timeout"),
    {next_state, StateName, State};
handle_info({branch_result, Pid, Branch, BranchState, #response{status=Status}=Response}, bye_sent=StateName, #state{bye_pid = Pid, bye_branch = Branch} = State) ->
    logger:log(normal, "branch_result: ~p ~p~n", [BranchState, Status]),
    if
        BranchState == completed, Status >= 200, Status =< 299 ->
	    logger:log(normal, "Terminate dialog: ~p ~p", [BranchState, Status]),
	    {stop, normal, State};
	BranchState == completed, Status >= 300, Status =< 699 ->
	    logger:log(normal, "Terminate dialog: ~p ~p", [BranchState, Status]),
            {stop, normal, State};
        true ->
            logger:log(normal, "IGNORING response '~p ~p ~s' to my BYE",
		       [BranchState, Status, Response#response.reason]),
            {next_state, StateName, State}
    end;
handle_info({branch_result, Pid, Branch, BranchState, #response{status=Status}=Response}, outgoing=StateName, #state{invite_pid = Pid, invite_branch = Branch} = State) ->
    logger:log(normal, "branch_result: ~p ~p~n", [BranchState, Status]),
    Call = State#state.call,
    if
        BranchState == terminated, Status >= 200, Status =< 299 ->
	    logger:log(normal, "Answered dialog: ~p ~p", [BranchState, Status]),
 	    Request = State#state.invite,
	    {ok, Dialog} =
		sipdialog:create_dialog_state_uac(Request, Response),
	    ok = sipdialog:register_dialog_controller(Dialog, self()),
	    ok = yate_call:answer(Call),

	    {ok, Ack, Dialog1} =
		generate_new_request("ACK", Dialog, State#state.contact,
				     State#state.invite_cseqno),
	    {ok, _SendingSocket, _Dst, _TLBranch} = send_ack(Ack),
	    State1 = State#state{dialog=Dialog1},
	    {next_state, up, State1};

%%  	    {next_state, StateName, State};
	BranchState == completed, Status >= 300, Status =< 699 ->
	    logger:log(normal, "Terminate dialog: ~p ~p", [BranchState, Status]),
	    ok = yate_call:drop(Call),
            {stop, normal, State};
	BranchState == proceeding, Status == 180 ->
	    logger:log(normal, "Ringing: ~p ~p", [BranchState, Status]),
	    ok = yate_call:ringing(Call),
            {next_state, StateName, State};
        true ->
            logger:log(normal, "IGNORING response '~p ~p ~s' to my invite",
		       [BranchState, Status, Response#response.reason]),
            {next_state, StateName, State}
    end;
handle_info({new_response, #response{status=Status}=Response, Origin, _LogStr}, StateName, State) when is_record(Origin, siporigin) ->
    logger:log(normal, "200OK retransmitt received '~p ~s' to my invite",
	       [Status, Response#response.reason]),
    {next_state, StateName, State};
handle_info({new_request, FromPid, Ref, #request{method="ACK"} = _NewRequest, _Origin, _LogStrInfo}, incoming=StateName, State) ->
    %% Don't answer ACK
    %% TODO update dialog timeout?
    FromPid ! {ok, self(), Ref},
%%     logger:log(normal, "Dialog received ACK"),
    {next_state, StateName, State};

handle_info({new_request, FromPid, Ref, NewRequest, _Origin, _LogStrInfo}, StateName, State) ->
    THandler = transactionlayer:get_handler_for_request(NewRequest),
    FromPid ! {ok, self(), Ref},
    {Action, NewDialog} = 
	case sipdialog:update_dialog_recv_request(NewRequest, State#state.dialog) of
	    {error, old_cseq} ->
		transactionlayer:send_response_handler(THandler, 500, "CSeq not higher than last requests");
	    {ok, NewDialog1} ->
		case NewRequest#request.method of
		    "BYE" ->                        
			%% answer BYE with 200 Ok
			transactionlayer:send_response_handler(THandler, 200, "Ok"),
			logger:log(normal, "Dialog ended by remote end (using BYE)"),
			ok = yate_call:drop(State#state.call, "Normal Clearing"),
			{stop, NewDialog1};
		    _ ->
			%% answer all unknown requests with 501 Not Implemented
			transactionlayer:send_response_handler(THandler, 501, "Not Implemented"),
			{next_state, NewDialog1}
		end
	end,

    case Action of
        next_state ->
            {next_state, StateName, State#state{dialog = NewDialog}};
        stop ->
            {stop, normal, State#state{dialog = NewDialog}}
    end;
handle_info(Info, StateName, State) ->
    error_logger:error_msg("~p: Unhandled info in ~p ~p~n",
			   [?MODULE, Info, StateName]),
    {next_state, StateName, State}.


terminate(Reason, _StateName, _State) ->
    error_logger:error_msg("~p: Terminated ~p~n", [?MODULE, Reason]),
    terminated.

%% send_request() ->

send_request(Request) ->
    Branch = siprequest:generate_branch(),
    Route = keylist:fetch('route', Request#request.header),
    TargetURI = Request#request.uri,
    Dst = case Route of
	      [] ->
		  [Dst1 | _] = sipdst:url_to_dstlist(TargetURI, 500, TargetURI),
		  Dst1;
	      [FirstRoute | _] ->
		  [FRC] = contact:parse([FirstRoute]),
		  FRURL = sipurl:parse(FRC#contact.urlstr),
		  [Dst1 | _] = sipdst:url_to_dstlist(FRURL, 500, TargetURI),
		  Dst1
	  end,
    Pid = transactionlayer:start_client_transaction(Request, Dst, Branch, ?DEFAULT_TIMEOUT, self()),
    {ok, Pid, Branch}.


send_ack(Request) ->
    Branch = siprequest:generate_branch(),
    send_ack(Request, Branch).

send_ack(Request, Branch) ->
    Route = keylist:fetch('route', Request#request.header),
    TargetURI = Request#request.uri,
    Dst = case Route of
	      [] ->
		  [Dst1 | _] = sipdst:url_to_dstlist(TargetURI, 500, TargetURI),
		  Dst1;
	      [FirstRoute | _] ->
		  [FRC] = contact:parse([FirstRoute]),
		  FRURL = sipurl:parse(FRC#contact.urlstr),
		  [Dst1 | _] = sipdst:url_to_dstlist(FRURL, 500, TargetURI),
		  Dst1
	  end,

    case transportlayer:send_proxy_request(none, Request, Dst,
					   ["branch=" ++ Branch]) of
	{ok, SendingSocket, TLBranch} ->
	    {ok, SendingSocket, Dst, TLBranch};
	_ ->
	    error
    end.
