%%%
%%% @doc       SIP REGISTER server
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
%%%
%%% States:
%%% unregistered - not registered, initial state 
%%% pending      - request send
%%% registered   - aor registered
%%%
-module(sipregister).

-behaviour(gen_fsm).

-define(DEFAULT_EXPIRE, 1 * 60).
-define(MIN_EXPIRE, 30).

-include("siprecords.hrl").
-include("sipsocket.hrl").

%% api
-export([build_register/1,
	 start_link/2,
	 start_link/1,
	 send_register/1,
	 send_unregister/1]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,

	 %% states
	 unregistered/2,
	 pending/2,
	 registered/2]).

-record(state, {
	  owner,				% Receives notifications
	  contact_urlstr,			% Contact url
	  reg,					% Initial request
	  reg_pending,				% Last request
	  reg_branch,				% Last branch
	  reg_pid,				% Last pid
	  reg_cseqno=1,				% Next CSeq number
	  retry_timer,				% 401/407 retry timer
	  rereg_timer,				% reREGISTER timer
	  auths=[]				% Authentication 
	 }).


%%--------------------------------------------------------------------
%% @spec build_register(Aor) -> {ok, Request}
%%           Aor = string()
%%           Request = request()
%% @doc Build REGISTER request for given SIP address (Aor)
%% @end
%%--------------------------------------------------------------------
build_register(Aor) when is_list(Aor) ->
    [From] = contact:parse([Aor]),
    {ok, Request, _CallId, _FromTag, _CSeqNo} =
	siphelper:start_generate_request("REGISTER",
                               From,
                               From,
                               [{"Expires", [integer_to_list(?DEFAULT_EXPIRE)]}],
			       <<>>
                              ),
    {ok, Request}.


%%--------------------------------------------------------------------
%% @spec start_link(Request) -> Result
%%           Request = request()
%%           Result = {ok, Pid}
%% @doc Start REGISTER process, calling process receives notifications
%%--------------------------------------------------------------------
start_link(Request) when is_record(Request, request) ->
    start_link(Request, self()).


%%--------------------------------------------------------------------
%% @spec start_link(Request, Owner) -> Result
%%           Request = request()
%%           Owner = pid()
%%           Result = {ok, Pid}
%% @doc Start REGISTER process, Owner receives notifications
%%--------------------------------------------------------------------
start_link(Request, Owner) when is_record(Request, request) ->
    gen_fsm:start_link(?MODULE, [Request, Owner], []).


%%--------------------------------------------------------------------
%% @spec send_register(Pid) -> Result
%%           Pid = pid()
%%           Result = ok
%% @doc Register SIP address
%%--------------------------------------------------------------------
send_register(Pid) when is_pid(Pid) ->
    gen_fsm:send_event(Pid, register).


%%--------------------------------------------------------------------
%% @spec send_unregister(Pid) -> Result
%%           Pid = pid()
%%           Result = ok
%% @doc Unregister SIP address
%%--------------------------------------------------------------------
send_unregister(Pid) when is_pid(Pid) ->
    gen_fsm:send_event(Pid, unregister).


%%--------------------------------------------------------------------
%% gen_fsm callbacks
%%
%% FIXME need to regenerate to-tag and call-id, if it has been
%% restarted by the supervisor
%% add username, password config
%%--------------------------------------------------------------------
init([Request, Owner]) when is_record(Request, request),
				 is_pid(Owner) ->
    Contacts_str = keylist:fetch('contact', Request#request.header),
    [Contact] = contact:parse(Contacts_str),
    Contact_urlstr = Contact#contact.urlstr,

    State = #state{owner=Owner,
		   contact_urlstr = Contact_urlstr,
		   reg=Request},

    {ok, State1} = do_send_register(Request, State),
    {ok, pending, State1}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


unregistered(unregister, State) ->
    {next_state, unregistered, State};
unregistered(register, State) ->
    Request = State#state.reg,
    {ok, State1} = do_send_register(Request, State),
    {next_state, pending, State1}.

pending(unregister, State) ->
    %% FIXME queue unregister?
    {next_state, pending, State};
pending(register, State) ->
    %% FIXME queue register?
    {next_state, pending, State}.


registered(unregister, State) ->
    {ok, State1} = do_send_unregister(State#state.reg, State),
    {next_state, pending, State1};
registered(register, State) ->
    {next_state, registered, State}.


handle_sync_event(Event, _From, StateName, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Event]),
    {reply, ok, StateName, State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(Request, StateName, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {next_state, StateName, State}.

handle_info({retry_reg, Request}, StateName, State) ->
    {ok, State1} = do_send_register(Request, State),
    {next_state, StateName, State1};

handle_info({reregister}, StateName, State) ->
    {ok, State1} = do_send_register(State#state.reg, State),
    {next_state, StateName, State1};

handle_info({clienttransaction_terminating, _Pid, _Branch}, StateName, State) ->
    {next_state, StateName, State};

handle_info({branch_result, Pid, Branch, _BranchState, #response{status=Status}=Response}, StateName, #state{reg_pid = Pid, reg_branch = Branch} = State) ->

    if
	% Provisional response
	Status >= 101, Status =< 199 ->
	    error_logger:info_msg("~p: Ignore provisional response ~n", [?MODULE]),
	    {next_state, StateName, State};

	% Success
	Status >= 200, Status =< 299 ->
	    %% FIXME use expires for first contact, to support NAT
	    Expires = find_expires(State#state.contact_urlstr, Response),
	    error_logger:info_msg("~p: Expires ~p~n", [?MODULE, Expires]),

	    {StateName1, State1} =
		if
		    Expires > 0 ->
			Timeout =
			    if
				Expires < ?MIN_EXPIRE ->
				    ?MIN_EXPIRE;
				true ->
				    Expires - ?MIN_EXPIRE
			    end,
			{ok, Rereg_timer} = timer:send_after(Timeout * 1000, {reregister}),
			State2 = State#state{rereg_timer=Rereg_timer},
			{registered, State2};
		    true ->
			{stop, State}
		end,

	    ok = send_event(StateName1, State1),
	    case StateName1 of
		stop ->
		    {stop, normal, State1};
		_ ->
		    {next_state, StateName1, State1}
	    end;

	%% TODO Handle 302 Moved Temporarily
%% 	Status >= 302 ->
%% 	    error_logger:info_msg("~p: Moved ~n", [?MODULE]),
%% 	    {next_state, registered, State};

	Status == 401; Status == 407 ->
	    Lookup = fun(Realm, From, To) ->
			     error_logger:info_msg("~p: fun ~p ~p ~p~n", [?MODULE, Realm, From, To]),
 			     {ok, "2001", "test"}
		     end,
	    {ok, Auths, Changed} = siphelper:update_authentications(Response, Lookup, State#state.auths),

	    Retry_after = siphelper:get_retry_after(Response) * 1000,

	    error_logger:info_msg("~p: Authenticate ~p~n", [?MODULE, Changed]),

	    case Changed of
		false ->
		    {stop, {siperror, Status, Response#response.reason}, State};
		true ->
		    Request = State#state.reg_pending,
%% 		    {ok, Request1} = siphelper:add_authorization(Request, Auths),
		    {ok, Retry_timer} = timer:send_after(Retry_after, {retry_reg, Request}),
		    State1 = State#state{retry_timer=Retry_timer, auths=Auths},
		    {next_state, StateName, State1}
	    end;

	% 403 Forbidden
	Status >= 403  ->
	    % TODO retry if stale nonce?
	    error_logger:info_msg("~p: Forbidden ~n", [?MODULE]),
	    {stop, {siperror, Status, Response#response.reason}, State};


	% 423 Interval to Brief
	Status == 423  ->
	    Retry_after = siphelper:get_retry_after(Response),
	    error_logger:info_msg("~p: Retry after ~p~n", [?MODULE, Retry_after]),
	    Request = State#state.reg_pending,
	    {ok, Retry_timer} = timer:send_after(Retry_after * 1000, {retry_reg, Request}),
	    State1 = State#state{retry_timer=Retry_timer},
	    {next_state, StateName, State1};

	% Retry 4xx and 5xx and 600
	Status >= 400, Status =< 599; Status == 600  ->
%% 	    Retry_after = siphelper:get_retry_after(Response),
%% 	    error_logger:info_msg("~p: Retry after ~p~n", [?MODULE, Retry_after]),
%% 	    {ok, _Timer} = timer:send_after(Retry_after * 1000, {retry_reg, State#state.reg_pending}),
%% 	    {next_state, StateName, State};
	    {stop, {siperror, Status, Response#response.reason}, State};

	% Global failure 6xx
	Status>= 600, Status =< 699 ->
	    error_logger:info_msg("~p: Global failure ~n", [?MODULE]),
	    {stop, {siperror, Status, Response#response.reason}, State}
    end;

handle_info(_Info, StateName, State) ->
    error_logger:error_msg("~p: Unhandled info in ~p~n",
			   [?MODULE, StateName]),
    {next_state, StateName, State}.


terminate(Reason, _StateName, _State) ->
    error_logger:error_msg("~p: Terminated ~p~n", [?MODULE, Reason]),
    terminated.


do_send_probe(Request, State) -> 
    Header1 = keylist:delete('contact', Request#request.header),
    Request1 = Request#request{header=Header1},
    do_send_register(Request1, State).

do_send_unregister(Request, State) -> 
    case State#state.rereg_timer of
	undefined ->
	    ok;
	Timer ->
	    {ok, cancel} = timer:cancel(Timer)
    end,
    State1 = State#state{rereg_timer=undefined},
    do_send_register(Request, State1, 0).

do_send_register(Request, State, Expires) when is_integer(Expires) ->
    Expires_str = integer_to_list(Expires),
    Header1 = keylist:set("Expires", [Expires_str], Request#request.header),
    Request1 = Request#request{header=Header1},
    do_send_register(Request1, State).

do_send_register(Request, State) ->
    CSeq = State#state.reg_cseqno,
    Header = Request#request.header,
    Header1 = keylist:set("CSeq", [lists:concat([CSeq, " ", Request#request.method])], Header), 

    Request1 = Request#request{header=Header1},
    {ok, Request2} = siphelper:add_authorization(Request1, State#state.auths),

    {ok, Pid, Branch} = siphelper:send_request(Request2),

    error_logger:info_msg("~p: Send request ~p ~p~n", [?MODULE, Pid, Branch]),

    State1 = State#state{reg_pending=Request1,	% Save before adding Auths
			 reg_branch=Branch,
			 reg_pid=Pid,
			 reg_cseqno=CSeq + 1
			},
    {ok, State1}.


send_event(NextStateName, State) ->
    Owner = State#state.owner,
    Reg = State#state.reg,
    Aor = keylist:fetch('to', Reg#request.header),
    Owner ! {NextStateName, self(), Aor},
    ok.


find_expires(Aor, Response) when is_list(Aor), is_record(Response, response) ->
    Contacts_str = keylist:fetch('contact', Response#response.header),
    Contacts = contact:parse(Contacts_str),
    find_expires(Aor, Response, Contacts).
    
find_expires(_Aor, _Response, []) ->
    0;
find_expires(Aor, Response, [#contact{urlstr=Aor}=Contact|_R]) ->
    Expires =
	case contact_param:find(Contact#contact.contact_param, "expires") of
	    [] ->
		[Expires1] = keylist:fetch('expires', Response),
		Expires1;
	    [Expires1] ->
		Expires1
	end,
    list_to_integer(Expires);
find_expires(Aor, Response, [Contact|R]) when is_record(Contact, contact) ->
    error_logger:info_msg("~p: No match ~p ~p~n", [?MODULE, Aor, Contact#contact.urlstr]),
    find_expires(Aor, Response, R).
