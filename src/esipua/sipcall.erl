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

-module(sipcall).

-behaviour(gen_fsm).


%% api
-export([
	 start/1,
	 start_link/1,
	 start_link/2,
	 start_link/3,
	 stop/1,
	 build_invite/3,
	 send_invite/2,
	 receive_invite/3,
	 proceeding/3,
	 proceeding/4,
	 answer/2,
	 answer/3,
	 drop/1,
	 drop/3
%%call/1
	]).

%% YXA behaviour
-export([
%% 	 init/0,
	 terminate/1,
%% 	 request/3,
	 response/3
	]).

%% behaviour
-export([behaviour_info/1]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,

	 %% States
	 start/2,
	 incoming/2,
	 outgoing/2,
	 up/2]).

-record(state, {dialog,				% Final SIP Dialog
		owner,
		early_dialogs=[],		% List of early dialogs
%% 		module,				% Behaviour module
%% 		options,			% Behaviour options
		invite_req,			% In/out INVITE request
		invite_pid,			% In/out INVITE pid
		invite_branch,			% Outgoing INVITE branch
		invite_cseqno=0,		% Outgoing cseq
		invite_rseqno,			% Incoming rseqno
		ack_req,			% ACK req
		bye_branch,			% BYE branch id
		bye_pid,			% BYE pid
		auths=[],
		retry_timer,
		contact				% Local contact
	       }).



-include("siprecords.hrl").
-include("sipsocket.hrl").

behaviour_info(callbacks) ->
    [{init, 1}];
behaviour_info(_Other) ->
    undefined.


terminate(_Mode) ->
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
		    CallId = sipheader:callid(Response#response.header),
		    case callregister:find_call(CallId) of
			{ok, Pid} ->
			    Pid ! {new_2xx_response, Response, Origin, LogStr};
			_ ->
			    logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction, no matching dialog, no matching call - dropping", [LogStr, Status, Reason])
		    end
	    end;
	true ->
            logger:log(normal, "sipclient: Response to ~s: '~p ~s', no matching transaction - dropping",
		   [LogStr, Status, Reason])
    end,
    ok.

%%
%% outgoing yate call
%%
%% call(From, To, Body) when is_record(From, contact),
%% 			  is_record(To, contact),
%% 			  is_binary(Body) ->
%%     start_link(From, To, Body).

%%
%% build_invite
%%
build_invite(From, To, Body) when is_record(From, contact),
				  is_record(To, contact),
				  is_binary(Body) ->
    {ok, Request, _CallId, _FromTag, _CSeqNo} =
	siphelper:start_generate_request("INVITE",
                               From,
                               To,
                               [
				{"Content-Type", ["application/sdp"]}
%% 				{"Require", ["100rel"]}
                               ],
			       Body
                              ),
    {ok, Request}.

send_invite(Pid, Request) when is_pid(Pid), is_record(Request, request) ->
    gen_fsm:send_all_state_event(Pid, {send_invite, Request}).

receive_invite(Call, Request, OldPid) when is_pid(Call),
					   is_record(Request, request),
					   is_pid(OldPid) ->
    gen_fsm:send_event(Call, {receive_invite, Request, OldPid}).

start(Request) when is_record(Request, request) ->
    Callid = list_to_atom(sipheader:callid(Request#request.header)),
    start(Callid);

start(Callid) when is_list(Callid) ->
    start(list_to_atom(fix_callid(Callid)));

start(Id) when is_atom(Id) ->
    IncomingSpec = {Id,
		    {sipcall, start_link,
		     [{local, Id}, [], self()]},
		    temporary, 2000, worker, [sipcall]},
    supervisor:start_child(esipua_sup, IncomingSpec).

start_link(Options) ->
    start_link(Options, self()).

start_link(Options, Owner) ->
    gen_fsm:start_link(?MODULE, [Owner], Options).

start_link(ServerName, Options, Owner) ->
    gen_fsm:start_link(ServerName, ?MODULE, [Owner], Options).

drop(Call) ->
    gen_fsm:send_event(Call, drop).

drop(Call, Status, Reason) when is_integer(Status),
				Status >= 400,
				Status =< 699,
				is_list(Reason) ->
    gen_fsm:send_event(Call, {drop, Status, Reason}).


proceeding(Call, Status, Reason) when is_pid(Call),
				      is_integer(Status),
				      Status >= 101,
				      Status =< 199,
				      is_list(Reason) ->
    proceeding(Call, Status, Reason, <<>>).

proceeding(Call, Status, Reason, Body) when is_pid(Call),
				      is_integer(Status),
				      Status >= 101,
				      Status =< 199,
				      is_list(Reason),
				      is_binary(Body) ->
    gen_fsm:send_event(Call, {proceeding, Status, Reason, Body}).


answer(Call, Body) when is_pid(Call),
			is_binary(Body) ->
    answer(Call, Body, []).

answer(Call, Body, ExtraHeaders) when is_pid(Call),
				      is_binary(Body),
				      is_list(ExtraHeaders) ->
    ok = gen_fsm:send_event(Call, {answer, ExtraHeaders, Body}).

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%
%% gen_fsm callbacks
%%
init([Owner]) ->
    process_flag(trap_exit, true),
    logger:log(normal, "~p: ~p~n", [?MODULE, self()]),
    link(Owner),
    {ok, start, #state{owner=Owner}}.

create_dialog(Request, Contact) ->
    THandler = transactionlayer:get_handler_for_request(Request),
    {ok, ToTag} = transactionlayer:get_my_to_tag(THandler),
    {ok, Dialog} = sipdialog:create_dialog_state_uas(Request, ToTag, Contact),
    ok = sipdialog:register_dialog_controller(Dialog, self()),
    {ok, Dialog}.

%% adopt_transaction(THandler, FromPid, ToPid) ->
%%     logger:log(normal, "sipclient: before change_parent ~p~n", [self()]),
%%     ok = transactionlayer:change_transaction_parent(THandler, FromPid, ToPid),
%%     logger:log(normal, "sipclient: after change_parent ~p~n", [self()]),
%%     ok.

send_response(State, Status, Reason) when is_record(State, state),
					  is_integer(Status),
					  is_list(Reason) ->
    send_response(State, Status, Reason, []).

send_response(State, Status, Reason, ExtraHeaders) when is_record(State, state),
							is_integer(Status),
							is_list(Reason),
							is_list(ExtraHeaders) ->
    send_response(State, Status, Reason, ExtraHeaders, <<>>).

send_response(State, Status, Reason, ExtraHeaders, Body) when is_record(State, state),
							      is_integer(Status),
							      is_list(Reason),
							      is_list(ExtraHeaders),
							      is_binary(Body)  ->
    Request = State#state.invite_req,
    Contact = State#state.contact,
    siphelper:send_response(Request, Status, Reason, ExtraHeaders, Body, Contact).


start({receive_invite, Request, OldPid}, State) ->
    case transactionlayer:adopt_st_and_get_branchbase(Request) of
	ignore ->
	    {stop, {error, ignore}};
	error ->
	    {stop, error};
	_BranchBase ->
	    THandler = transactionlayer:get_handler_for_request(Request),
	    Pid = transactionlayer:get_pid_from_handler(THandler),
	    ok = transactionlayer:change_transaction_parent(THandler, OldPid, self()),

	    CallId = sipheader:callid(Request#request.header),
	    {_DisplayName, ToURI} = sipheader:to(Request#request.header),

	    %% FIXME Contact
	    Contact_url = sipurl:new([{proto, "sip"},
				      {user, ToURI#sipurl.user},
				      {host, siphost:myip()},
				      {port, sipsocket:get_listenport(udp)}]),
	    Contact = contact:new(Contact_url),
	    Contact_str = contact:print(Contact),
	    {ok, Dialog} = create_dialog(Request, Contact_str),

	    ok = callregister:register_call(CallId, self()),
	    State0 = State#state{invite_req=Request, invite_pid=Pid,
				contact=Contact_str, dialog=Dialog},
	    {next_state, incoming, State0}
    end.


%% Cancel outgoing call
outgoing(drop, State) ->
    Invite_pid = State#state.invite_pid,
    ExtraHeaders = [],
    Invite_pid ! {cancel, "hangup", ExtraHeaders},

    {stop, normal, State};
outgoing({drop, Status, Reason}, State) ->
    Invite_pid = State#state.invite_pid,
    ExtraHeaders = [{"Reason", [lists:concat(["SIP ;cause=", Status, " ;text=\"", Reason, "\""])]}],
    Invite_pid ! {cancel, "hangup", ExtraHeaders},
    {stop, normal, State}.


%% Drop incoming call
incoming(drop, State) ->
    outgoing({drop, 403, "Forbidden"}, State);
incoming({drop, Status, Reason}, State) when is_integer(Status),
						   Status >= 400,
						   Status =< 699,
						   is_list(Reason) ->
    ok = send_response(State, Status, Reason),
    {next_state, incoming, State};

%% Indicate proceeding to incoming call
incoming({proceeding, Status, Reason, Body}, State) when is_integer(Status),
						   Status >= 101,
						   Status =< 199,
						   is_list(Reason),
						   is_binary(Body) ->
    ok = send_response(State, Status, Reason, [], Body),
    {next_state, incoming, State};

%% Answer incoming call
incoming({answer, ExtraHeaders, Body}, State) when is_list(ExtraHeaders),
						   is_binary(Body) ->
    ok = send_response(State, 200, "Ok", ExtraHeaders, Body),
    {next_state, up, State}.


%% Drop connected call
up(drop, State) ->
    up({drop,  200, "Normal Clearing"}, State);
up({drop, Status, Reason}, State) when is_integer(Status),
				       is_list(Reason) ->
    {ok, State1} = do_send_bye(Status, Reason, State),
    {next_state, bye_sent, State1}.


%% Generate and send BYE
do_send_bye(Status, Reason, State) ->
    ExtraHeaders = [{"Reason", [lists:concat(["SIP ;cause=", Status, " ;text=\"", Reason, "\""])]}],
    Dialog = State#state.dialog,

    {ok, Bye, Dialog1} =
	siphelper:generate_new_request("BYE", Dialog, State#state.contact, ExtraHeaders),
    {ok, Pid, Branch} = siphelper:send_request(Bye),

    State1 = State#state{dialog=Dialog1, bye_pid=Pid, bye_branch=Branch},
    {ok, State1}.



code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


handle_sync_event(Event, _From, StateName, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Event]),
    {reply, ok, StateName, State}.


handle_event(stop, _StateName, State) ->
    {stop, normal, State};

%% Send initial INVITE
handle_event({send_invite, Request}, start, State) ->
    [Contact] = keylist:fetch('contact', Request#request.header),
    CallId = sipheader:callid(Request#request.header),
    ok = callregister:register_call(CallId, self()),
    State0 = State#state{invite_req=Request, contact=Contact},
    case do_send_invite(Request, State0) of
	{ok, State1} ->
	    {next_state, outgoing, State1};
	{siperror, _Status1, _Reason1} ->
	    {stop, normal, State};
	R ->
	    logger:log(normal, "send_request failed: ~p", [R]),
	    {stop, normal, State}
    end;

handle_event(Request, StateName, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {next_state, StateName, State}.



%% Server transaction cancelled by UAC
handle_info({servertransaction_cancelled, Pid, ExtraHeaders}, incoming=_StateName, #state{invite_pid=Pid}=State) ->
    %% TODO Send hangup
    logger:log(normal, "servertransaction_cancelled ~p~n", [ExtraHeaders]),
    ok = send_response(State, 487, "Request Terminated"),

    Owner = State#state.owner,
    Owner ! {call_drop, self(), ExtraHeaders},
    
    {stop, normal, State};

%% Ignore server transaction termination
handle_info({servertransaction_terminating, InvitePid}, StateName, #state{invite_pid=InvitePid}=State) ->
    %% Ignore
%%     logger:log(normal, "servertransaction_terminating ~p ~p~n", [InvitePid, StateName]),
    {next_state, StateName, State};

%% Ignore client transaction termination
handle_info({clienttransaction_terminating, InvitePid, _Branch}, StateName, #state{invite_pid=InvitePid}=State) ->
%%     logger:log(normal, "clienttransaction_terminating ~p ~p~n", [InvitePid, StateName]),
    {next_state, StateName, State};
handle_info(timeout, incoming=StateName, State) ->
    %% TODO Handle INVITE timeout
    ok = send_response(State, 408, "Request Timeout"),
    {next_state, StateName, State};

%% BYE response
%% TODO handle BYE response from SIP core?
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

%% INVITE response from UAS
handle_info({branch_result, Pid, Branch, BranchState, #response{status=Status}=Response}, outgoing=StateName, #state{invite_pid = Pid, invite_branch = Branch} = State) ->
    {Skip, State1} = pred_skip_resp(BranchState, Status, Response, State),
    case Skip of
	false ->
	    handle_invite_result(Pid, Branch, BranchState, Response, StateName, State1);
	true ->
	    {next_state, StateName, State1}
    end;

%% INVITE response from SIP core
handle_info({branch_result, Pid, Branch, BranchState, {_Status, _Reason}=Response}, outgoing=StateName, #state{invite_pid = Pid, invite_branch = Branch} = State) ->
    handle_invite_result(Pid, Branch, BranchState, Response, StateName, State);
handle_info({new_response, Response, Origin, _LogStr}, StateName, State) when is_record(Origin, siporigin) ->
    case siphelper:cseq(Response#response.header) of
	{CSeqNo, "INVITE"} when CSeqNo == State#state.invite_cseqno,
			        State#state.ack_req /= undefined ->
	    %% Resend ACK
	    {ok, _SendingSocket, _Dst, _TLBranch} = siphelper:send_ack(State#state.ack_req, State#state.auths);
	_ ->
	    error_logger:error_msg("~p: Ignore new response ~p ~p~n", [?MODULE, Response#response.status, Response#response.reason])
    end,
    {next_state, StateName, State};

%% 2xx response from other fork
handle_info({new_2xx_response, Response, Origin, LogStr}, StateName, State) when is_record(Origin, siporigin) ->
    handle_new_2xx_response(Response, Origin, LogStr, StateName, State);

%% New in-dialog ACK
handle_info({new_request, FromPid, Ref, #request{method="ACK"} = _NewRequest, _Origin, _LogStrInfo}, StateName, State) ->
    %% Don't answer ACK
    %% TODO update dialog timeout?
    %% FIXME stop retransmission of 200 Ok
    FromPid ! {ok, self(), Ref},
%%     logger:log(normal, "Dialog received ACK"),
    {next_state, StateName, State};

%% New in-dialog request
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
			Owner = State#state.owner,
			Owner ! {call_drop, self(), NewRequest},
			{stop, NewDialog1};
%% 		    "INVITE" ->
%% 			%% reINVITE
%% 			ok = send_response(State, 200, "Ok"),
%% 			Owner = State#state.owner,
%% 			Owner ! {call_request, self(), NewRequest},
%% 			{next_state, NewDialog1};
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

handle_info({retry_invite, Request}, StateName, State) ->
    case do_send_invite(Request, State) of
	{ok, State1} ->
	    {next_state, StateName, State1};
	{siperror, _Status1, _Reason1} ->
	    {stop, normal, State};
	R ->
	    logger:log(normal, "send_request failed: ~p", [R]),
	    {stop, normal, State}
    end;

handle_info({dialog_expired, {CallId, LocalTag, RemoteTag}=DialogId}, StateName, State) ->
    %% TODO compare with existing early or confirmed dialogs
    error_logger:info_msg("~p: Refresh dialog ~p~n", [?MODULE, DialogId]),
    sipdialog:set_dialog_expires(CallId, LocalTag, RemoteTag, 30),

%%     case compare_dialog(DialogId, State#state.dialog) of
%% 	{ok, Dialog} ->
%% 	    %% Update established dialog
%% 	    sipdialog:set_dialog_expires(Dialog, 30);
%% 	error ->
%% 	    %% Terminate all dialogs
%% 	    Early = State#state.early_dialogs,
%% 	    case find_dialog(DialogId, Early) of
%% 		{ok, Dialog1} ->
%% 		    Early1 = drop_dialog(Dialog1, Early),
%% 		    xxxxxxxxxxxx

    {next_state, StateName, State};

handle_info({'EXIT', Owner, Reason}, StateName, #state{owner=Owner}=State) ->
    error_logger:error_msg("~p: Owner ~p terminated ~p~n",
			   [?MODULE, Owner, Reason]),
    {ok, State1} = shutdown(StateName, State),
    {stop, Reason, State1};

handle_info({'EXIT', _Pid, normal}, StateName, State) ->
    %% ignore
    {next_state, StateName, State};

handle_info({'EXIT', _Pid, Reason}, StateName, State) ->
    {ok, State1} = shutdown(StateName, State),
    {stop, Reason, State1};

handle_info(Info, StateName, State) ->
    error_logger:error_msg("~p: Unhandled info in ~p ~p~n",
			   [?MODULE, Info, StateName]),
    {next_state, StateName, State}.


terminate(Reason, _StateName, _State) ->
    error_logger:error_msg("~p: Terminated ~p~n", [?MODULE, Reason]),
    terminated.

%%
%% Internal
%%

%% Terminate ongoing session
shutdown(start, State) ->
    {ok, State};
shutdown(incoming, State) ->
    ok = send_response(State, 500, "Server Internal Error"),
    {ok, State};
shutdown(outgoing, State) ->
    %% TODO Wait for 200ok and send ack and bye
    {ok, State};
shutdown(up, State) ->
    %% TODO Wait for 200ok to BYE
    {ok, State1} = do_send_bye(500, "Server Internal Error", State),
    {ok, State1};
shutdown(bye_sent, State) ->
    %% TODO Wait for 200ok to BYE
    {ok, State}.


%% If reliable response, skip if already received, otherwise update state
%% and send PRACK
%% If unreliable, never skip
pred_skip_resp(BranchState, Status, Response, State) when BranchState == proceeding, Status >= 101, Status =< 199 ->
%	    Request = Status#state.invite,
    case sipheader:is_required("100rel", Response#response.header) of
	true ->
	    [Rseq_str] = keylist:fetch("rseq", Response#response.header),
	    Rseq = list_to_integer(Rseq_str),
	    Inv_rseq = State#state.invite_rseqno,
	    Dialog = State#state.dialog,
	    
	    logger:log(normal, "Rseq ~p ~p ~p~n", [Rseq_str, Rseq, Dialog]),
	    if
		Inv_rseq == undefined ->
		    logger:log(normal, "undefined rseq~n", []),
		    {ok, State1} = send_prack(Response, Rseq, State),
		    {false, State1};
		Inv_rseq + 1 == Rseq ->
		    logger:log(normal, "Next rseq~n", []),
		    {ok, State1} = send_prack(Response, Rseq, State),
		    {false, State1};
		true ->
		    logger:log(normal, "Not next rseq~n", []),
		    {true, State}
	    end;
	false ->
	    logger:log(normal, "no 100rel~n", []),
	    {false, State}
    end;
pred_skip_resp(_BranchState, _Status, _Response, State) ->
    {false, State}.


%% Send PRACK for received response
send_prack(Response, Rseq, State) ->
    {Dialog, State0} = need_dialog(Response, State),
    {ok, Request, NewDialog} = siphelper:generate_new_request("PRACK", Dialog, State0#state.contact),

%%     throw({send_prack, Dialog, Request}),

    Rack = lists:concat([Rseq, " ", State0#state.invite_cseqno, " INVITE"]),
    Header1 = keylist:set("RAck", [Rack], Request#request.header),
    
    {ok, _Pid, _Branch} = siphelper:send_request(Request#request{header=Header1}),
    Early = update_dialogs(NewDialog, State0#state.early_dialogs),
    State1 = State0#state{invite_rseqno=Rseq, early_dialogs=Early},

    % TODO add Pid to outgoing pid list
    {ok, State1}.


%% set_dialog_state_uac(Response, State) when is_record(Response, response),
%% 					   is_record(State, state) ->
%%     case State#state.dialog of
%% 	_ ->
%% 	    {ok, Dialog1} = create_dialog_state_uac(State#state.invite_req, Response),
%% 	    Dialog1;
	
%% 	Dialog1 ->
%%  	    update_dialog_state_uac(Response, Dialog1)
%%     end.

%% Create and register dialog as UAC
create_dialog_state_uac(Request, Response) when is_record(Request, request),
						is_record(Response, response) ->
    error_logger:info_msg("~p: create_dialog_state_uac1 ~n", [?MODULE]),
    {ok, Dialog} = sipdialog:create_dialog_state_uac(Request, Response),
    ok = sipdialog:register_dialog_controller(Dialog, self()),
    error_logger:info_msg("~p: create_dialog_state_uac2 ~n", [?MODULE]),
    Dialog.

%% Update dialog record route from 2xx
update_dialog_state_uac(Response, Dialog) when is_record(Response, response),
						is_record(Dialog, dialog) ->
    %% Update route set of existing dialog
    RHeader = Response#response.header,
    Route = lists:reverse(keylist:fetch('record-route', RHeader)),
    Dialog1 = Dialog#dialog{route_set=Route},
    refresh_dialog_target_uac(Response, Dialog1).


%% Refresh dialog target for target refresh requests
refresh_dialog_target_uac(Response, Dialog) when is_record(Response, response),
						is_record(Dialog, dialog) ->
    %% Update route set of existing dialog
    RHeader = Response#response.header,
    RemoteTarget =
        case {Dialog#dialog.remote_target, keylist:fetch('contact', RHeader)} of
	    {undefined, []} -> undefined;
            {Contact1, []} -> Contact1;
            {_, [Contact1]} -> Contact1
        end,

    Dialog#dialog{remote_target=RemoteTarget}.


%%
%% Handle INVITE responses from UAS or SIP core.
%%
handle_invite_result(Pid, Branch, BranchState, {Status, Reason}=_Response, StateName, State) ->
    handle_invite_result(Pid, Branch, BranchState, #response{status=Status, reason=Reason}, Status, Reason, StateName, State);

handle_invite_result(Pid, Branch, BranchState, #response{status=Status, reason=Reason}=Response, StateName, State) ->
    handle_invite_result(Pid, Branch, BranchState, Response, Status, Reason, StateName, State).

handle_invite_result(Pid, Branch, BranchState, Response, Status, Reason, StateName, State) ->
    logger:log(normal, "branch_result: ~p ~p~n", [BranchState, Status]),
    Owner = State#state.owner,
    if
	%% TODO Handle all 101 <= Status <= 199
	Status >= 101, Status =< 199 ->
	    {_Dialog, State1} = need_dialog(Response, State),

	    Owner ! {call_proceeding, self(), Response},

            {next_state, StateName, State1};
        Status >= 200, Status =< 299 ->
	    logger:log(normal, "Answered dialog: ~p ~p", [BranchState, Status]),
	    handle_invite_2xx(Pid, Branch, BranchState, Response, StateName, State);
	Status >= 300, Status =< 399 ->
	    Owner ! {call_redirect, self(), Response},
            {stop, normal, State};

	State#state.invite_cseqno == 1, Status == 401 orelse Status == 407 ->
	    Lookup = fun(Realm, From, To) ->
			     error_logger:info_msg("~p: fun ~p ~p ~p~n", [?MODULE, Realm, From, To]),
 			     {ok, "2001", "test"}
		     end,
	    {ok, Auths, Changed} = siphelper:update_authentications(Response, Lookup, State#state.auths),

	    Retry_after = siphelper:get_retry_after(Response) * 1000,

	    case Changed of
		false ->
		    Owner ! {call_drop, self(), Response},
		    {stop, normal, State};
		true ->
		    Request = State#state.invite_req,
		    {ok, Retry_timer} = timer:send_after(Retry_after, {retry_invite, Request}),
		    State1 = State#state{retry_timer=Retry_timer, auths=Auths},
		    {next_state, StateName, State1}
	    end;
%% 	    {ok, Request} = siphelper:add_authorization(State#state.invite, Response),

	Status >= 400, Status =< 699 ->
	    Owner ! {call_drop, self(), Response},

            {stop, normal, State};
        true ->
            logger:log(normal, "IGNORING response '~p ~p ~s' to my invite",
		       [BranchState, Status, Reason]),
            {next_state, StateName, State}
    end.


handle_invite_2xx(_Pid, _Branch, _BranchState, Response, outgoing=_StateName, State) when is_record(Response, response) ->
%%     case sipheader:dialogid(Response#response.header) of
%% 	xxxx
%%     Dialog =
%% 	case find_dialog(Response, State) of
%% 	    {early, Dialog2} ->
%% 		update_dialog_state_uac(Response, Dialog2);

    Owner = State#state.owner,
    Early = State#state.early_dialogs,
    Dialog =
	case find_dialog(Response, Early) of
	    {ok, Dialog2} ->
		update_dialog_state_uac(Response, Dialog2);
	    error ->
		create_dialog_state_uac(State#state.invite_req, Response)
	end,

    Early1 = drop_dialog(Dialog, Early),

    %% FIXME wait 64T1 and drop early dialogs
    {ok, Ack, Dialog1} =
	siphelper:generate_new_request("ACK", Dialog, State#state.contact,
			     State#state.invite_cseqno),
%%     {ok, _SendingSocket, _Dst, _TLBranch} = send_ack(Ack, State#state.auths),

    Owner ! {call_answered, self(), Response},

    State1 = State#state{dialog=Dialog1, early_dialogs=Early1,
			 ack_req=Ack},
    {next_state, up, State1}.


%% Handle aditional 2xx responses received from other UAS:s than first
%% ACK dialog and terminate
%% Should be done in a process on its own.
handle_new_2xx_response(#response{status=Status}=Response, _Origin, _LogStr, up=StateName, State) ->
    logger:log(normal, "Another 200 OK received '~p ~s' to my invite",
	       [Status, Response#response.reason]),

    Early = State#state.early_dialogs,
    Dialog =
	case find_dialog(Response, Early) of
	    {ok, Dialog2} ->
		update_dialog_state_uac(Response, Dialog2);
	    error ->
		create_dialog_state_uac(State#state.invite_req, Response)
	end,

    Early1 = drop_dialog(Dialog, Early),

    ok = sipdialog:unregister_dialog_controller(Dialog),
    sipcall_bye:start_link(Dialog, State#state.contact, State#state.auths,
			  State#state.invite_cseqno),

    State1 = State#state{early_dialogs=Early1},
    {next_state, StateName, State1}.


%% Send first INVITE or retry if 401/407 was received
do_send_invite(Request, State) ->
    [Contact] = keylist:fetch('contact', Request#request.header),

    %% TODO set cseqno

    Header = Request#request.header,
    CSeq = State#state.invite_cseqno + 1,
    Header1 = keylist:set("CSeq", [lists:concat([CSeq, " ", Request#request.method])], Header),
    Request1 = Request#request{header=Header1},
    {ok, Request2} = siphelper:add_authorization(Request1, State#state.auths),

    case siphelper:send_request(Request2) of
	{ok, Pid, Branch} ->
	    
	    State1 = State#state{invite_branch=Branch,
				 invite_pid=Pid,
				 invite_req=Request1,
				 invite_cseqno=CSeq,
				 contact=Contact
				},
	    {ok, State1};
	R ->
	    R
    end.


%% Create a dialog if not already done
need_dialog(Response, State) when is_record(Response, response) ->
    Early = State#state.early_dialogs,
    case find_dialog(Response, Early) of
	{ok, Dialog1} ->
	    %% Ignore
	    logger:log(normal, "Early dialog exists, ignore", []),
	    {Dialog1, State};
	error ->
	    Dialog1 = create_dialog_state_uac(State#state.invite_req, Response),
	    Early1 = add_dialog(Dialog1, Early),
	    
	    State1 = State#state{early_dialogs=Early1},
	    {Dialog1, State1}
    end.


%% Update matching dialog in list
update_dialogs(Dialog, Dialogs) when is_record(Dialog, dialog),
				     is_list(Dialogs) ->
    Update = fun(D) when is_record(D, dialog) ->
		     case compare_dialog(Dialog, D) of
			 true ->
			     Dialog;
			 false ->
			     D
		     end
	     end,

    lists:map(Update, Dialogs).


%% Remove matching dialog from list
drop_dialog(Dialog, Dialogs) when is_record(Dialog, dialog),
				  is_list(Dialogs) ->
    lists:filter(fun (D) ->
			 not compare_dialog(D, Dialog)
		 end, Dialogs).


%% Find for response or dialog id
find_dialog(_DialogId, []) ->
    error;

find_dialog(Response, Dialogs) when is_record(Response, response),
				     is_list(Dialogs) ->
    Header = Response#response.header,
    DialogId = sipheader:dialogid(Header),
    find_dialog(DialogId, Dialogs);

find_dialog({CallId, LocalTag, RemoteTag}=DialogId, [Dialog|R]) when is_list(CallId),
			     is_list(LocalTag),
			     is_list(RemoteTag),
			     is_record(Dialog, dialog) ->

    case compare_dialog(DialogId, Dialog) of
	true ->
	    {ok, Dialog};
	false ->
	    find_dialog(DialogId, R)
    end.


%% Add dialog to list
add_dialog(Dialog, Dialogs) when is_record(Dialog, dialog),
				 is_list(Dialogs) ->
    [Dialog | Dialogs].


%% Compare dialog id:s
compare_dialog(D1, D2) when is_record(D1, dialog),
			    is_record(D2, dialog) ->
    D1#dialog.callid == D2#dialog.callid andalso
	D1#dialog.local_tag == D2#dialog.local_tag andalso
	D1#dialog.remote_tag == D2#dialog.remote_tag;

compare_dialog({CallId, LocalTag, RemoteTag}, D2) when is_list(CallId),
						       is_list(LocalTag),
						       is_list(RemoteTag),
						       is_record(D2, dialog) ->
    CallId == D2#dialog.callid andalso
	LocalTag == D2#dialog.local_tag andalso
	RemoteTag == D2#dialog.remote_tag.

fix_callid(Callid) when is_list(Callid) ->
    Fun = fun(E) ->
		  if
		      E == $@ ->
			  $_;
		      E == $. ->
			  $_;
		      E == $- ->
			  $_;
		      true ->
			  E
		  end
	  end,

    lists:map(Fun, Callid).
