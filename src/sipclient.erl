%%
%% Based on sipclient.erl from Yxa
%%

-module(sipclient).

-behaviour(gen_server).


%% api
-export([start_link/3, stop/0, process/1]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {dialog, invite_pid, invite, bye_pid, bye_branch, handle, id, owner, peerid,
	       address, port, sdp_body, contact}).

-define(SERVER, ?MODULE).


-export([init/0, request/3, response/3]).
-export([test/0, start_generate_request/5]).

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
        logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction - proxying statelessly",
		   [LogStr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response),
    ok.

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

    URL = Request#request.uri,
    case sipdst:url_to_dstlist(URL, 500, URL) of
        [Dst | _] ->
            BranchBase = siprequest:generate_branch(),
            BranchSeq = 1,
            Branch = lists:concat([BranchBase, "-UAC-", BranchSeq]),
	    _LogFun = undefined,
	    Timeout = ?DEFAULT_TIMEOUT,
	    _Pid = transactionlayer:start_client_transaction(Request, Dst, Branch, Timeout, self())
    end,
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
    %% Figure out a bunch of parameters in ways that vary depending on if we have
    {ok, CSeqNum, NewDialog} = sipdialog:get_next_local_cseq(Dialog),
    [C] = contact:parse([NewDialog#dialog.remote_target]),
    Route = case NewDialog#dialog.route_set of
		[] ->
		    [];
		Route1 ->
		    [{"Route", Route1}]
	    end,
    To = contact:new(none, NewDialog#dialog.remote_uri,
		[{"tag", NewDialog#dialog.remote_tag}]),
    logger:log(normal, "Remote URI: ~p", [To]),
    TargetURI = sipurl:parse(C#contact.urlstr),
    From = contact:new(none, NewDialog#dialog.local_uri,
			      [{"tag", NewDialog#dialog.local_tag}]),
    Header = keylist:from_list([{"From",        [contact:print(From)]},
				{"To",          [contact:print(To)]},
                                {"Call-Id",     [NewDialog#dialog.callid]},
                                {"CSeq",        [lists:concat([CSeqNum, " ", Method])]},
				{"Contact",     [Contact]}
                               |Route]),
    Request1 = #request{method = Method,
                        uri    = TargetURI,
                        header = Header
                       },
    Request = siprequest:set_request_body(Request1, <<>>),
    {ok, Request, NewDialog}.




%%--------------------------------------------------------------------
%% Function: start_generate_request(Method, Referer, Referee, ExtraHeaders, Body)
%%           Method       = string(), SIP method
%%           Referer      = contact record()
%%           Referee      = contact record()
%%           ExtraHeaders = list() of {Key, ValueList} tuple()
%%           Body         = binary(), request body
%% Descrip.: Part of the startup functions. Build our initial request
%%           record.
%% Returns : {ok, Request, CallId, FromTag}
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
				{"CSeq",	[CSeq ++ " " ++ Method]},
				{"CSeq",	[lists:concat([CSeq, " ", Method])]}
			       ] ++ ExtraHeaders),

    URI = sipurl:parse(To#contact.urlstr),
    Request1 = #request{method = Method, uri = URI, header = Header},
    Request = siprequest:set_request_body(Request1, Body),

    {ok, Request, CallId, FromTag}.




start_link(Client, Request, LogStr) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    THandler = transactionlayer:get_handler_for_request(Request),
    case  gen_server:start_link(?MODULE, [Client, Request, LogStr], []) of
	{ok, Pid} ->
	    ok = adopt_transaction(THandler, Pid),
	    {ok, Pid};
	{error, _Reason} ->
	    error;
	ignore ->
	    ignore
    end.

adopt_transaction(THandler, Pid) ->
    STPid = transactionlayer:get_pid_from_handler(THandler),
    ok = gen_server:call(STPid, {change_parent, self(), Pid}),
    ok.

%%
%% Return when the transaction has completed or terminated.
%%
process(_Pid) ->
%%     gen_server:call(Pid, process, 30000).
    ok.
    

stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([Client, Request, LogStr]) ->
    case transactionlayer:adopt_st_and_get_branchbase(Request) of
	ignore ->
	    {stop, {error, ignore}};
	error ->
	    {stop, error};
	BranchBase ->
	    init2(Client, Request, LogStr, BranchBase)
    end.

init2(Client, Request, LogStr, _BranchBase) ->
    {ok, Handle} = yate:open(Client),
    logger:log(normal, "sipclient: INVITE ~s ~p~n", [LogStr, self()]),
    {ok, Address, Port} = parse_sdp(Request),
    THandler = transactionlayer:get_handler_for_request(Request),
    Invite_pid = transactionlayer:get_pid_from_handler(THandler),
    State = #state{invite=Request, invite_pid=Invite_pid,
		   handle=Handle, address=Address, port=Port},
%%     {ok, _TRef} = timer:send_after(20000, timeout),
    execute(State).

parse_sdp(Request) ->
    logger:log(normal, "sipclient: foo~n"),
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
    logger:log(normal, "sipclient: connection ~s ~p~n", [Address, Port]),
    {ok, Address, Port}.

execute(State) ->
    Call_to = "dumb/",
    Handle = State#state.handle,
    Request = State#state.invite,
    Uri = Request#request.uri,
    Target = Uri#sipurl.user,
    ok = yate:watch(Handle, call.progress,
		    fun(_Cmd) ->
			    true
		    end),
    ok = yate:watch(Handle, call.ringing,
		    fun(_Cmd) ->
			    true
		    end),
    ok = yate:watch(Handle, call.drop,
		    fun(_Cmd) ->
			    true
		    end),
    ok = yate:watch(Handle, chan.disconnected,
		    fun(_Cmd) ->
			    true
		    end),
    ok = yate:watch(Handle, call.answered,
		    fun(_Cmd) ->
			    true
%% 			    Id == dict:fetch(id, Cmd#command.keys)
		    end),
    {ok, RetValue, RetCmd} =
	yate:send_msg(Handle, call.execute,
		      [
		       {caller, "1234"},
		       {callername, "mikael"},
		       {callto, Call_to},
		       {target, Target}
		      ]),
    logger:log(normal, "sipclient: RetCmd ~p~n", [dict:to_list(RetCmd#command.keys)]),
    case RetValue of
	false ->
	    %% TODO return false
	    ok = send_response(Request, 404, "Not found"),
	    ignore;
	true ->
	    Id = dict:fetch(id, RetCmd#command.keys),
	    State1 = State#state{id=Id},
	    {ok, State2} = setup(State1),
	    {ok, State2}
    end.
	    

setup(State) ->   
    Handle = State#state.handle,
    Request = State#state.invite,
    Id = State#state.id,
    ok = yate:watch(Handle, chan.hangup,
		    fun(Cmd) ->
			    Id == dict:fetch(id, Cmd#command.keys)
		    end),

    %% FIXME Contact
    Contact = "<sip:dummy@192.168.0.4:5080>",
    {ok, Dialog} = create_dialog(Request, Contact),

%%     ExtraHeaders = [
%% 		    {"Contact", [Contact]}
%% 		   ],
%%     ok = send_response(Request, 102, "Dialog Creation", ExtraHeaders),

    {ok, State1b} = startup(State, Id),
    {ok, State1b#state{contact=Contact,dialog=Dialog}}.

startup(State, Id) ->
    {ok, _RetValue, RetCmd} =
	yate:send_msg(State#state.handle, chan.masquerade,
		      [
		       {message, "chan.startup"},
		       {id, Id},
		       {driver, "erlang"}
		      ]),
%%     Peer_id = dict:fetch(peerid, RetCmd#command.keys),
%%     {ok, State#state{peerid=Peer_id}}.
    {ok, State}.
    

get_sdp_body(State) ->
    case State#state.sdp_body of
	undefined ->
	    {ok, State1} = start_rtp(State, State#state.id),
	    {ok, State1, State1#state.sdp_body};
	_ ->
	    {ok, State, State#state.sdp_body}
    end.

start_rtp(State, Id) ->
    {ok, _RetValue, RetCmd} =
	yate:send_msg(State#state.handle, chan.masquerade,
		      [
		       {message, "chan.attach"},
		       {id, Id},
		       {notify, tag},
		       {source, "rtp/*"},
 		       {consumer, "rtp/*"},
		       {remoteip, State#state.address},
		       {remoteport, State#state.port},
		       {format, "alaw"}
		      ]),

    case dict:find(localport, RetCmd#command.keys) of
	{ok, Local_port} ->
	    logger:log(normal, "sipclient: local rtp port ~p", [Local_port]);
	_ ->
	    ok
    end,

    Localip = dict:fetch(localip, RetCmd#command.keys),
    Localport = list_to_integer(dict:fetch(localport, RetCmd#command.keys)),

    {ok, Body} = create_sdp_body(Localip, Localport),
    {ok, State#state{sdp_body=Body}}.

%%     ExtraHeaders = [
%% 		    {"Contact", [Contact]}
%% 		   ],
%%     Request = State#state.invite,
%%     ok = send_response(Request, 183, "Session Progress", ExtraHeaders, Body),

answer(State, Id) ->
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(State#state.handle, chan.masquerade,
		      [
		       {message, "call.answered"},
		       {id, Id}
		      ]),
    ok.

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
    Request = State#state.invite,
    Contact = State#state.contact,
    {ok, State1, Body} = get_sdp_body(State),
    ExtraHeaders = [
		    {"Contact", [Contact]}
		   ],
    ok = send_response(Request, 200, "Ok", ExtraHeaders, Body),
    {ok, State1}.

send_response(Request, Status, Reason) ->
    send_response(Request, Status, Reason, []).

send_response(Request, Status, Reason, ExtraHeaders) ->
    send_response(Request, Status, Reason, ExtraHeaders, <<>>).

send_response(Request, Status, Reason, ExtraHeaders, Body) ->
    transactionlayer:send_response_request(Request, Status, Reason, ExtraHeaders, Body).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(process, From, State) ->
    {noreply, State#state{owner=From}};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.

handle_info({servertransaction_cancelled, Pid, _ExtraHeaders}, #state{invite_pid=Pid}=State) ->
    %% FIXME
    logger:log(normal, "servertransaction_cancelled ~n", []),
    ok = send_response(State#state.invite, 487, "Request Terminated"),
    ok = drop(State#state.handle, State#state.id, "Cancelled"),
    {noreply, State};
handle_info({servertransaction_terminating, Pid}, #state{invite_pid=Pid}=State) ->
    %% FIXME
    logger:log(normal, "servertransaction_terminating ~n", []),
    {noreply, State};
handle_info(timeout, State) ->
    ok = send_response(State#state.invite, 408, "Request Timeout"),
    ok = drop(State#state.handle, State#state.id, "Request Timeout"),
%%     gen_server:reply(State#state.owner, ok),
    {noreply, State};
handle_info({yate, Dir, Cmd, From}, State) ->
    handle_command(Cmd#command.type, Dir, Cmd, From, State);
handle_info({branch_result, Pid, Branch, BranchState, #response{status=Status}=Response}, #state{bye_pid = Pid, bye_branch = Branch} = State) ->
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
            {noreply, State}
    end;
handle_info({new_request, FromPid, Ref, #request{method="ACK"} = _NewRequest, _Origin, _LogStrInfo}, State) ->
    %% Don't answer ACK
    %% TODO update dialog timeout?
    FromPid ! {ok, self(), Ref},
%%     logger:log(normal, "Dialog received ACK"),
    {noreply, State};

handle_info({new_request, FromPid, Ref, NewRequest, _Origin, _LogStrInfo}, State) ->
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
			ok = drop(State#state.handle, State#state.id, "Normal Clearing"),
			{stop, NewDialog1};
		    _ ->
			%% answer all unknown requests with 501 Not Implemented
			transactionlayer:send_response_handler(THandler, 501, "Not Implemented"),
			{noreply, NewDialog1}
		end
	end,

    case Action of
        noreply ->
            {noreply, State#state{dialog = NewDialog}};
        stop ->
            {stop, normal, State#state{dialog = NewDialog}}
    end;
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
%%     error_logger:error_msg("Terminated~n", [?MODULE]),
    terminated.

%% send_request() ->

drop(Handle, Id, Reason) ->
    {ok, _RetValue, _RetCmd} =
	yate:send_msg(Handle, call.drop,
		      [
		       {id, Id},
		       {reason, Reason}
		      ]),
    ok.

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


handle_command(message, Dir, Cmd, From, State) ->
    Name = (Cmd#command.header)#message.name,
    handle_message(Name, Dir, Cmd, From, State).

handle_message(chan.hangup, ans, Cmd, _From, State) ->
    Id = dict:fetch(id, Cmd#command.keys),
    error_logger:info_msg("Call hangup ~p~n", [Id]),
    %% TODO distinguish CANCEL and BYE
    %% Send bye
    {ok, Bye, NewDialog} = generate_new_request("BYE", State#state.dialog,
					       State#state.contact),
    {ok, Pid, Branch} = send_request(Bye),
    {noreply, State#state{dialog=NewDialog,bye_pid=Pid,bye_branch=Branch}};
handle_message(call.answered, ans, Cmd, _From, State) ->
    Id = dict:fetch(id, Cmd#command.keys),
    error_logger:info_msg("Handle answer ~p~n", [Id]),
    {ok, State1} = send_200ok(State),
    {noreply, State1};
handle_message(call.ringing, ans, _Cmd, _From, State) ->
    %% FIXME send sdp if earlymedia=true
    ok = send_response(State#state.invite, 180, "Ringing"),
    {noreply, State};
handle_message(chan.disconnected, ans, Cmd, _From, State) ->
    Id = dict:fetch(id, Cmd#command.keys),
    YReason = dict:fetch(reason, Cmd#command.keys),
    error_logger:info_msg("Call disconnect ~p ~p~n", [Id, YReason]),
    %% TODO distinguish CANCEL and BYE
    %% Send bye
%%     {ok, Bye, NewDialog} = generate_new_request("BYE", State#state.dialog,
%% 					       State#state.contact),
%%     {ok, Pid, Branch} = send_request(Bye),
%%     {noreply, State#state{dialog=NewDialog,bye_pid=Pid,bye_branch=Branch}};
    {Status, Reason} =
	case YReason of
	    "noroute" ->
		{404, "Not Found"};
	    "busy" ->
		{486, "Busy Here"};
%% 	    "forbidden" ->
%% 		{};
	    _ ->
		{500, "Internal Server Error"}
	end,

    ok = send_response(State#state.invite, Status, Reason),
    {noreply, State}.
