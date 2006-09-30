-module(siphelper).

-define(DEFAULT_TIMEOUT, 50).
-define(DEFAULT_RETRY_AFTER, 1).

-include("siprecords.hrl").
-include("sipclient.hrl").
%% -include("sipsocket.hrl").
%% -include("yate.hrl").
%% -include("sdp.hrl").

-export([
	 start_generate_request/5,
	 send_request/1,
	 send_response/3, send_response/4, send_response/5, send_response/6,
	 update_authentications/3,
	 update_authentications/5,
	 add_authorization/2, print_auth_response/8, get_retry_after/1
	]).

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

    ToURI = sipurl:parse(To#contact.urlstr),

    % TODO autodetect listenport proto
    Contact_url = sipurl:new([{proto, "sip"},
			      {user, ToURI#sipurl.user},
			      {host, siphost:myip()},
			      {port, sipsocket:get_listenport(udp)}]),
    Contact = contact:new(Contact_url),

    FromContact = contact:add_param(From, "tag", FromTag),
    Header = keylist:from_list([{"From",	[contact:print(FromContact)]},
				{"To",		[contact:print(To)]},
				{"Call-Id",	[CallId]},
				{"CSeq",	[lists:concat([CSeq, " ", Method])]},
%% 				{"Require",	["100rel"]},
				{"Contact",	[contact:print(Contact)]}
			       ] ++ ExtraHeaders),

    URI =
	case Method of
	    "REGISTER" ->
		sipurl:set([{user, none}], ToURI);
	    _ ->
		ToURI
	end,
    
    Request1 = #request{method = Method, uri = URI, header = Header},
    Request = siprequest:set_request_body(Request1, Body),

    {ok, Request, CallId, FromTag, CSeq}.


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


send_response(Request, Status, Reason) ->
    send_response(Request, Status, Reason, []).

send_response(Request, Status, Reason, ExtraHeaders) ->
    send_response(Request, Status, Reason, ExtraHeaders, <<>>).

send_response(Request, Status, Reason, ExtraHeaders, Body) when is_record(Request, request) ->
    transactionlayer:send_response_request(Request, Status, Reason, ExtraHeaders, Body).


send_response(Request, Status, Reason, ExtraHeaders, Body, undefined) when is_record(Request, request) ->
    send_response(Request, Status, Reason, ExtraHeaders, Body);
send_response(Request, Status, Reason, ExtraHeaders, Body, _Contact) when is_record(Request, request), Status > 299 ->
    send_response(Request, Status, Reason, ExtraHeaders, Body);
send_response(Request, Status, Reason, ExtraHeaders, Body, Contact) when is_record(Request, request), Status =< 299 ->
    ExtraHeaders1 = [{"Contact", [Contact]}] ++ ExtraHeaders,
    send_response(Request, Status, Reason, ExtraHeaders1, Body).


%%
%% update_authentications(Response, Lookup, Auths) -> Res
%% Response = response()
%% Lookup = fun(Realm, From, To) -> {ok, Username, Password}|noauth
%% Realm = string()
%% From = string()
%% To = string()
%% Username = string()
%% Password = string()
%% Auths = [sipauth()]
%% Res = {ok, Auths, Changed}
%% Changed = bool()
%%
update_authentications(Response, Lookup, Auths) when is_record(Response, response),
						     is_list(Auths) ->
    Proxy_auths = keylist:fetch('proxy-authenticate', Response#response.header),
    {ok, Auths1, Changed1} = update_authentications('proxy-authenticate', Response, Proxy_auths, Lookup, Auths),

    WWW_auths = keylist:fetch('www-authenticate', Response#response.header),
    error_logger:info_msg("~p: update_authentications ~p~n", [?MODULE, WWW_auths]),
    {ok, Auths2, Changed2} = update_authentications('www-authenticate', Response, WWW_auths, Lookup, Auths1),
    {ok, Auths2, Changed1 or Changed2}.

%%
%% update_authentications(Type, AuthHdrs, Auths) -> Res
%% Type = atom()
%% AuthHdrs = [string()]
%% Auths = [sipauth()]
%% Res = {Auths, Changed}
%% Changed = bool()
%%
update_authentications(Type, Response, AuthHdrs, Lookup, Auths) ->
    update_authentications(Type, Response, AuthHdrs, Lookup, Auths, false).

%%
%% update_authentications(Type, AuthHdrs, Auths, Changed) -> Res
%% Type = atom()
%% AuthHdrs = [string()]
%% Auths = [sipauth()]
%% Changed = bool()
%% Res = {ok, Auths, Changed}
%%
update_authentications(_Type, _Response, [], _Lookup, Auths, Changed) ->
    {ok, Auths, Changed};
update_authentications(Type, Response, [AuthHdr|R], Lookup, Auths, Changed) ->
    error_logger:info_msg("~p: update_authentication ~p~n", [?MODULE, AuthHdr]),

    AuthDict = sipheader:auth(AuthHdr),

    Fun = fun(Auth, {Changed2, Found2}) ->
		  {ok, Auth1, Changed3, Found3} = update_authentication(Type, AuthDict, Auth),
		  {Auth1, {Changed2 or Changed3, Found2 or Found3}}
	  end,
%%     Fun = fun(X, Y) -> {X, Y} end,
    
    {Auths1, {Changed1, Found1}} =
 	lists:mapfoldl(Fun, {Changed, false}, Auths),

    error_logger:info_msg("~p: update_authentication ~p ~p~n", [?MODULE, Changed1, Found1]),
    {Auths2, Changed3} =
	if
	    not Found1 ->
		SipAuth = sipauth_new(Type, Response, AuthDict, Lookup),
		{[SipAuth | Auths1], true};
	    true ->
		{Auths1, Changed1}
	end,
    
    update_authentications(Type, Response, R, Lookup, Auths2, Changed3).


sipauth_new(Type, Response, AuthDict, Lookup) ->
    %% Create new Auth record
    %% Use Lookup

    From = keylist:fetch('from', Response#response.header),
    To = keylist:fetch('to', Response#response.header),
    Realm = dict:fetch("realm", AuthDict),
    Stale =
	case dict:find("stale", AuthDict) of
	    {ok, Stale1} ->
		Stale1;
	    error ->
		false
	end,

    {User, Pass} =
	case Lookup(Realm, From, To) of
	    {ok, User1, Pass1} ->
		{User1, Pass1};
	    noauth ->
		{"anonymous", ""}
	end,
    #sipauth{type=Type, realm=Realm, dict=AuthDict, stale=Stale,
	     username=User, password=Pass}.

%%
%% update_authentication(Type, AuthDict, Auth) -> Res
%% Type = atom()
%% AuthDict = dict()
%% Auth = sipauth()
%% Res = {ok, Auth, Changed=bool(), Found=bool()}
%%
update_authentication(Type, [], Auth) when is_atom(Type),
					   is_list(Auth) ->
    {ok, Auth, false, false};

update_authentication(Type, AuthDict, #sipauth{type=Type,realm=Realm}=Auth) when is_atom(Type) ->
    case dict:find("realm", AuthDict) of
	{ok, Realm} ->
	    %% TODO update
	    Auth1 = Auth#sipauth{dict=AuthDict},
	    Stale =
		case dict:find("stale", AuthDict) of
		    {ok, Stale1} ->
			case httpd_util:to_lower(Stale1) of
			    "false" ->
				false;
			    "true" ->
				true
			end;
		    error ->
			false
		end,
			
	    {ok, Auth1, Stale, true};
	{ok, Realm1} ->
	    %% Realm not found or not matching
	    error_logger:info_msg("~p: Not match ~p ~p~n", [?MODULE, Realm, Realm1]),
	    {ok, Auth, false, false};
	_ ->
	    %% Realm not found or not matching
	    error_logger:info_msg("~p: Not match, error ~p~n", [?MODULE, Realm]),
	    {ok, Auth, false, false}
    end.


add_authorization(Request, Auths) when is_record(Request, request),
				       is_list(Auths) ->
    Header = Request#request.header,
    add_authorization(Request, Auths, Header).

add_authorization(Request, [], Header) when is_record(Request, request) ->
    {ok, Request#request{header=Header}};

add_authorization(Request, [Auth|R], Header) when is_record(Request, request), is_record(Auth, sipauth) ->

    {ok, Header1} = add_authorization2(Request, Header, Auth),

    add_authorization(Request, R, Header1).


add_authorization2(Request, Header, Auth) when is_record(Request, request),
					       is_record(Header, keylist),
					       is_record(Auth, sipauth) ->
    Type = Auth#sipauth.type,
    Dict = Auth#sipauth.dict,
    Realm = Auth#sipauth.realm,
    Nonce = dict:fetch("nonce", Dict),
    Method = case Request#request.method of
		 "ACK" ->
		     "INVITE";
		 Method1 ->
		     Method1
	     end,
    URIstr = sipurl:print(Request#request.uri),
    Opaque = case dict:find("opaque", Dict) of
		 {ok, Opaque1} ->
		     Opaque1;
		 error ->
		     ""
	     end,
    User = Auth#sipauth.username,
    Password = Auth#sipauth.password,
    
    Resp = sipauth:get_response(Nonce, Method, URIstr, User, Password, Realm),
    Resp_str = print_auth_response("Digest", User, Realm, URIstr, Resp, Nonce, Opaque, "MD5"),

    Name = case Type of
	       'proxy-authenticate' ->
		   "Proxy-Authorization";
	       'www-authenticate' ->
		   "Authorization"
	   end,

    {ok, keylist:prepend({Name, [Resp_str]}, Header)}.

%%keylist:prepend({"Proxy-Authorization", [Resp_str]}, Request#request.header),

%% 	    case siphelper:send_request(Request1) of
%% 		{ok, Pid1, Branch1} ->
%% 		    State1 = State#state{invite=Request1,
%% 					 invite_branch=Branch1,
%% 					 invite_pid=Pid1,
%% 					 invite_cseqno=CSeq},
%% 		    {next_state, outgoing, State1};
%% 		{siperror, Status1, Reason1} ->
%% 		    {stop, normal, State};
%% 		R ->
%% 		    logger:log(normal, "send_request failed: ~p", [R]),
%% 		    {stop, normal, State}
%% 	    end.

%%--------------------------------------------------------------------
%% Function: print_auth_response(AuthMethod, User, Realm, URIstr,
%%                               Response, Nonce, Opaque, Algorithm)
%%           All parameters are of type string()
%% Descrip.: Construct a challenge response, given a bunch of in-
%%           parameters.
%% Returns : string()
%%--------------------------------------------------------------------
print_auth_response(AuthMethod, User, Realm, URIstr, Response, Nonce, Opaque, Algorithm) ->
    
    Quote = "\"",
    QuoteComma = "\",",
    
    lists:concat([AuthMethod, " ",
                  "username=",          Quote, User,            QuoteComma,
                  "realm=",             Quote, Realm,           QuoteComma,
                  "uri=",               Quote, URIstr,          QuoteComma,
                  "response=",          Quote, Response,        QuoteComma,
                  "nonce=",             Quote, Nonce,           QuoteComma,
                  "opaque=",            Quote, Opaque,          QuoteComma,
                  "algorithm=",         Algorithm]).

%%
%%
%%
get_retry_after(Response) when is_record(Response, response) ->
    case keylist:fetch('retry-after', Response#response.header) of
	[Retry_after1] ->
	    case catch list_to_integer(Retry_after1) of
		Retry_after2 when is_integer(Retry_after2) ->
		    Retry_after2;
		_ ->
		    ?DEFAULT_RETRY_AFTER
	    end;
	_ ->
	    ?DEFAULT_RETRY_AFTER
    end.

