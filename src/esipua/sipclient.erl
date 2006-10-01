%%
%% Based on sipclient.erl from Yxa
%%

%%
%% States:
%% incoming - Incoming calls 
%% outgoing - Outgoing calls
%% up       - Call up
%% bye_sent - SIP BYE sent
%%

-module(sipclient).

-behaviour(gen_fsm).


%% api
-export([start_link/3, stop/0, call/4]).

-export([make/0]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3]).

-record(state, {dialog,				% SIP Dialog
		invite,				% INVITE request
		sip_call,			% SIP call (Pid)
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
-export([test/0]).
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
    Callregister = {callregister, {callregister, start_link, []},
		    permanent, 2000, worker, [callregister]},
    Tables = [],
    [Tables, stateful, {append, [Server, Callregister]}].

request(#request{method="OPTIONS"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    logger:log(normal, "sipclient: Options ~s", [LogStr]),
    siphelper:send_response(Request, 200, "Ok");
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
	siphelper:start_generate_request(Method,
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


start_link(Client, Request, LogStr) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    gen_fsm:start_link(?MODULE, [Client, Request, LogStr, self()], []).


start_link(Client, Id, Cmd, From, Args) ->
    logger:log(normal, "sipclient: start_link ~p~n", [self()]),
    gen_fsm:start_link(?MODULE, [Client, Id, Cmd, From, Args], []).


stop() ->
    error.

%%
%% gen_fsm callbacks
%%

%% sipcall
init([]) ->
    {ok, undefined};


init([Client, Request, LogStr, OldPid]) ->
    process_flag(trap_exit, true),
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
		     {ok, []} ->
			 none;
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
    Contact = CallerUri,

    State = #state{client=Client,handle=Handle,call=Call,contact=Contact},
    Body = <<>>,

    {ok, Request} = sipcall:build_invite(FromHdr, ToHdr, Body),

    State2 = State#state{invite=Request, sdp_body=Body},
    {ok, outgoing, State2}.

init2(Client, Request, LogStr, _BranchBase, _OldPid) ->
    {ok, Handle} = yate:open(Client),
    logger:log(normal, "sipclient: INVITE ~s ~p~n", [LogStr, self()]),
    {ok, Address, Port} = parse_sdp(Request),

    %% TODO handle incoming call, build sipcall Pid
    State = #state{invite=Request,
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
    catch case yate_call:execute_link(State#state.client,
			       [
				{caller, Caller},
				{callername, Caller_name},
				{callto, Call_to},
				{target, Target}
			       ]) of
	{error, {noroute, _Cmd}} ->
	    %% FIXME reason, drop sip_call
%% 	    ok = send_response(State, 404, "Not Found"),
	    {stop, normal};
	{ok, Call} ->
	    execute_finish(Call, State)
    end.

execute_finish(Call, State) ->
    %% FIXME send 101 to sip_call
%%     ok = send_response(State, 101, "Dialog Establishment"),
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

start_rtp_receiver(State, Remote_addr) ->
    Call = State#state.call,
    {ok, Localip, Localport} =
	yate_call:start_rtp(Call, Remote_addr),

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
%% send_200ok(State) ->
%%     {ok, State1, Body} = get_sdp_body(State),
%%     ok = send_response(State1, 200, "Ok", [], Body),
%%     {ok, State1}.


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


handle_info({yate_call, execute, Call}, outgoing=StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: execute ~p~n", [?MODULE, StateName]),
    Remote_addr = "192.168.0.1",
    {ok, State1} = start_rtp_receiver(State, Remote_addr),
    Request = State1#state.invite,
    Body = State1#state.sdp_body,

    Request1 = siprequest:set_request_body(Request, Body),
    {ok, Sip_call} = sipcall:start_link(?MODULE, [], []),
    ok = sipcall:send_invite(Sip_call, Request1),

    State2 = State1#state{sip_call=Sip_call,
			  invite=Request1},
    {next_state, StateName, State2};

handle_info({yate_call, dialog, Call}, incoming=StateName, State=#state{call=Call}) ->
    %% FIXME send 101 to sip_call
%%     ok = send_response(State, 101, "Dialog Establishment"),
    {next_state, StateName, State};

handle_info({yate_call, ringing, Call}, incoming=StateName, State=#state{call=Call}) ->
    %% FIXME send sdp if earlymedia=true
    %% FIXME send 180 to sip_call
%%     ok = send_response(State, 180, "Ringing"),
    {next_state, StateName, State};

handle_info({yate_call, progress, Call}, incoming=StateName, State=#state{call=Call}) ->
    %% FIXME send 183 to sip_call
%%     {ok, State1, Body} = get_sdp_body(State),
%%     ok = send_response(State1, 183, "Session Progress", [], Body),
    {next_state, StateName, State};

handle_info({yate_call, answered, Call}, incoming=_StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: autoanswer ~n", [?MODULE]),
    %% FIXME send answered to sip_call
%%     {ok, State1} = send_200ok(State),
    {next_state, up, State};

handle_info({yate_call, disconnected, Cmd, Call}, StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: Call disconnected ~p ~p~n", [?MODULE, Call, StateName]),
    SipCall = State#state.sip_call,
    case command:find_key(reason, Cmd) of
	{ok, YateReason} ->
	    Status = reason_to_sipstatus(YateReason),
	    ok = sipcall:drop(SipCall, Status, "FIXME");
	_ ->
	    ok = sipcall:drop(SipCall)
	end,
    {stop, normal, State};

handle_info({yate_call, hangup, Call}, StateName, State=#state{call=Call}) ->
    error_logger:info_msg("~p: Call hangup ~p ~p~n", [?MODULE, Call, StateName]),
    {next_state, StateName, State};

handle_info(timeout, incoming=StateName, State) ->
    %% FIXME drop sip_call
%%     ok = send_response(State#state.invite, 408, "Request Timeout"),
    ok = yate_call:drop(State#state.call, "Request Timeout"),
    {next_state, StateName, State};


%% new_request "BYE":
%% ok = yate_call:drop(State#state.call, "Normal Clearing"),
%% {stop, NewDialog1};

handle_info({call_drop, SipCall, Response}, _StateName, #state{sip_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.call,
    Status = Response#response.status,
    Reason = sipstatus_to_reason(Status),
    ok = yate_call:drop(Call, Reason),
    {stop, normal, State};

handle_info({call_ringing, SipCall, Response}, outgoing=StateName, #state{sip_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.call,
    ok = yate_call:ringing(Call),
    {next_state, StateName, State};

handle_info({call_redirect, SipCall, Response}, outgoing=StateName, #state{sip_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.call,
    ok = yate_call:drop(Call, forbidden),
    {next_state, StateName, State};

handle_info({call_answered, SipCall, Response}, outgoing=StateName, #state{sip_call=SipCall}=State) when is_record(Response, response) ->
    Call = State#state.call,
    ok = yate_call:answer(Call),
    {next_state, up, State};

handle_info({'EXIT', Pid, Reason}, _StateName, #state{sip_call=Pid}=State) ->
    %% sip call terminated
    case Reason of
	{siperror, Status, _SipReason} ->
	    Call = State#state.call,
 	    YateReason = sipstatus_to_reason(Status),
	    ok = yate_call:drop(Call, YateReason),
	    {stop, normal, State};
	_ ->
	    {stop, Reason}
    end;

handle_info({'EXIT', _Pid, normal}, StateName, State) ->
    %% Ignore normal exit
    {next_state, StateName, State};

handle_info({'EXIT', _Pid, Reason}, _StateName, State) ->
    %% Terminate with error
    {stop, Reason, State};

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


sipstatus_to_reason(401) ->
    noauth;
sipstatus_to_reason(403) ->
    forbidden;
sipstatus_to_reason(404) ->
    noroute;
%% sipstatus_to_reason(404) ->
%%     offline;
sipstatus_to_reason(406) ->
    rejected;
sipstatus_to_reason(415) ->
    nomedia;
sipstatus_to_reason(480) ->
    congestion;
sipstatus_to_reason(483) ->
    looping;
sipstatus_to_reason(481) ->
    nocall;
sipstatus_to_reason(484) ->
    incomplete;
sipstatus_to_reason(486) ->
    busy;
sipstatus_to_reason(487) ->
    noanswer;
sipstatus_to_reason(491) ->
    pending;
sipstatus_to_reason(Status) when Status >= 400, Status =< 499 ->
    failure;

sipstatus_to_reason(503) ->
    noconn;
sipstatus_to_reason(Status) when Status >= 500, Status =< 599 ->
    failure;

sipstatus_to_reason(603) ->
    forbidden;
sipstatus_to_reason(606) ->
    rejected;
sipstatus_to_reason(604) ->
    noroute;
sipstatus_to_reason(Status) when Status >= 600, Status =< 699 ->
    busy.

reason_to_sipstatus(incomplete) ->
    484;
reason_to_sipstatus(noroute) ->
    404;
reason_to_sipstatus(noconn) ->
    503;
reason_to_sipstatus(noauth) ->
    401;
reason_to_sipstatus(nomedia) ->
    415;
reason_to_sipstatus(nocall) ->
    481;
reason_to_sipstatus(busy) ->
    486;
reason_to_sipstatus(noanswer) ->
    487;
reason_to_sipstatus(rejected) ->
    406;
reason_to_sipstatus(forbidden) ->
    403;
reason_to_sipstatus(offline) ->
    404;
reason_to_sipstatus(congestion) ->
    480;
reason_to_sipstatus(failure) ->
    500;
reason_to_sipstatus(pending) ->
    491;
reason_to_sipstatus(looping) ->
    483;
reason_to_sipstatus(Reason) ->
    error_logger:error_msg("~p: Unknown reason code '~p', returning 500~n",
			   [?MODULE, Reason]),
    500.

%% {404, "Not Found"};
%% {486, "Busy Here"};
%% {403, "Forbidden"};
%% {500, "Internal Server Error"}



make() ->
    Modules = [
	        "callregister",
	        "register_server",
	        "register_sup",
		"sdp",
		"sipcall",
		"sipclient",
		"sipregister",
	        "siptest",
	        "siphelper",
		"ysip_srv"
	    ],

    Prefix = "../../../src/esipua/",
    Files = lists:map(fun(File) -> Prefix ++ File end, Modules),

    make:files(Files,
	       [load,
		{i, "../../../include"},
		{i, "/usr/lib/yxa/include"},
		{outdir, "../../src/esipua"},
		debug_info]).
