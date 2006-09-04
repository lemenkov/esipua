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

-record(state, {dialog, invite, bye_pid, bye_branch, handle, id, owner}).

-define(SERVER, ?MODULE).


-export([init/0, request/3, response/3]).
-export([test/0, start_generate_request/5]).

-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("yate.hrl").

-define(DEFAULT_TIMEOUT, 50).
-define(HOST, "localhost").
-define(PORT, 15062).

init() ->
    Server = {ysip_srv, {ysip_srv, start_link, [?HOST, ?PORT]},
	      permanent, 2000, worker, [ysip_srv]},
    Tables = [],
    [Tables, stateful, {append, [Server]}].

request(#request{method="OPTIONS"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
%%     THandler = transactionlayer:get_handler_for_request(Request),
%%    LogTag = get_branchbase_from_handler(THandler),
%%     To = keylist:fetch('to', Request#request.header),
%%     [ToContact] = contact:parse(To),
%%     ToTag = "dummy-tag",
%%     TaggedTo = contact:add_param(ToContact, "tag", ToTag),
    logger:log(normal, "sipclient: Options ~s", [LogStr]),
    send_response(Request, 200, "Ok");
request(#request{method="INVITE"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    %% Send 404
    %% send_response(Request, 603, "Decline");
    %% Start dialog and send 200 Ok
    ok = ysip_srv:invite(Request, LogStr),
    ok;
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
%%  #contact{display_name = none,
%% 		       urlstr = "sip:contact@localhost",
%% 		       contact_param = contact_param:to_norm([])
%% 		      },
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
%% Function: generate_new_request(Method, State)
%%           Method = string(), SIP method
%%           State  = state record()
%% Descrip.: Generate a request template using values from the dialog
%%           state in State#state.dialog, or from the INVITE request
%%           created during startup and stored in
%%           State#state.invite_request (note that the INVITE request
%%           is always created, even though it is not always sent).
%% Returns : {ok, Request, NewDialog}
%%--------------------------------------------------------------------
generate_new_request(Method, Dialog) ->
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
    Contact = "sip:dummy2@localhost",
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
    {ok, Pid} = gen_server:start_link(?MODULE, [Client, Request, LogStr], []),
    {ok, Pid}.

%%
%% Return when the transaction has completed or terminated.
%%
process(Pid) ->
    gen_server:call(Pid, process).
    

stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([Client, Request, LogStr]) ->
    {ok, Handle} = yate:open(Client),
    logger:log(normal, "sipclient: INVITE ~s ~p~n", [LogStr, self()]),
    State = #state{invite=Request, handle=Handle},
    ok = send_response(Request, 101, "Proceeding"),
    {ok, _TRef} = timer:send_after(2000, timeout),
    {ok, State}.
%%     {ok, State1} = send_102(State),
%%     {ok, State1}.
%%     {ok, State}.
%%     {ok, State1} = send_200ok(State),
%%     {ok, State1}.
%%     {ok, Id} = execute(State),
%%     logger:log(normal, "sipclient: execute ~p~n", [Id]),
%%     {ok, State#state{id=Id}}.

execute(State) ->
    Call_to = "tone/silence",
    Target = "dumb/",
    Handle = State#state.handle,
    {ok, _RetValue, RetCmd} =
	yate:send_msg(Handle, call.execute,
		      [
		       {callto, Call_to},
		       {direct, Target}
		      ]),
    logger:log(normal, "sipclient: RetCmd ~p~n", [dict:to_list(RetCmd#command.keys)]),
    Id = dict:fetch(id, RetCmd#command.keys),
    ok = yate:watch(Handle, chan.hangup,
		    fun(Cmd) ->
			    Id == dict:fetch(id, Cmd#command.keys)
		    end),
    ok = yate:watch(Handle, call.answered,
		    fun(Cmd) ->
			    Id == dict:fetch(id, Cmd#command.keys)
		    end),
    {ok, Id}.

send_200ok(State) ->
    Request = State#state.invite,
    THandler = transactionlayer:get_handler_for_request(Request),
%%    LogTag = get_branchbase_from_handler(THandler),
    {ok, ToTag} = transactionlayer:get_my_to_tag(THandler),
    To = keylist:fetch('to', Request#request.header),
    [ToContact] = contact:parse(To),
    logger:log(normal, "sipclient: INVITE ~p ~p", [To, THandler]),
    Contact = "sip:dummy@localhost",

    TaggedTo = contact:add_param(ToContact, "tag", ToTag),
    Header = keylist:from_list([{"To",	    [contact:print(TaggedTo)]},
				{"Contact", [Contact]}]),
    Response = #response{header=Header},
    {ok, Dialog} = sipdialog:create_dialog_state_uas(Request, Response),
    %%DialogId = {Dialog#dialog.callid, Dialog#dialog.local_tag, Dialog#dialog.remote_tag},
    ok = sipdialog:register_dialog_controller(Dialog, self()),
    ok = send_response(Request, 200, "Ok"),
    {ok, #state{dialog=Dialog}}.


send_response(Request, Status, Reason) ->    
    THandler = transactionlayer:get_handler_for_request(Request),
    ExtraHeaders = [],
    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders).


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


handle_info(timeout, State) ->
    ok = send_response(State#state.invite, 408, "Request Timeout"),
    gen_server:reply(State#state.owner, ok),
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
            {stop, {error, Status}, State};
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
    {ok, Request, NewDialog} = generate_new_request("BYE", State#state.dialog),
    {ok, Pid, Branch} = send_request(Request),
    {noreply, State#state{dialog=NewDialog,bye_pid=Pid,bye_branch=Branch}};

%%    {noreply, State};
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
    terminated.

%% send_request() ->

send_request(Request) ->
    Branch = siprequest:generate_branch(),
    Timeout = 30,

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
    {stop, normal, State};
handle_message(chan.answer, ans, Cmd, _From, State) ->
    Id = dict:fetch(id, Cmd#command.keys),
    error_logger:info_msg("Handle answer ~p~n", [Id]),
    {noreply, State}.
