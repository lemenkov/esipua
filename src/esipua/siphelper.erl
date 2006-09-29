-module(siphelper).

-define(DEFAULT_TIMEOUT, 50).
-define(DEFAULT_RETRY_AFTER, 2).

-include("siprecords.hrl").
%% -include("sipsocket.hrl").
%% -include("yate.hrl").
%% -include("sdp.hrl").

-export([
	 start_generate_request/5,
	 send_request/1,
	 send_response/3, send_response/4, send_response/5, send_response/6,
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

%% -record(sipauth, {
%% 	  type,
%% 	  realm,
%% 	  domain,
%% 	  opaque,
%% 	  stale,
%% 	  algorithm,
%% 	  qop,
%% 	  username,
%% 	  password
%% 	 }).

%% update_authentication(Auths, AuthHeader) ->
%%     ok.

%% update_authentication() ->


add_authorization(Request, Response) ->
	    %% TODO fix looping
%% 	    CSeq = State#state.invite_cseqno + 1,
    CSeq = "Dummy",
	    %% TODO loop auth list
    Header = Request#request.header,
    Proxy_auth = keylist:fetch('proxy-authenticate', Response#response.header),
    {ok, Header1} = add_authorization2(Request, Proxy_auth, "Proxy-Authorization", Header),

    WWW_auth = keylist:fetch('www-authenticate', Response#response.header),
    {ok, Header2} = add_authorization2(Request, WWW_auth, "Authorization", Header1),
%%     Header2 = keylist:set("CSeq", [lists:concat([CSeq, " ", Method])], Header1), 

    Request1 = Request#request{header=Header2},
    {ok, Request1}.

add_authorization2(Request, Auths, Name, Header) ->
    case add_authorization2(Request, Auths, []) of
	{ok, []} ->
	    {ok, Header};
	{ok, Header1} ->
	    {ok, keylist:prepend({Name, Header1}, Header)}
    end.

add_authorization2(_Request, [], Headers) ->
    {ok, Headers};

add_authorization2(Request, [Auth|R], Headers) ->
    Dict = sipheader:auth(Auth),
    Realm = dict:fetch("realm", Dict),
    Nonce = dict:fetch("nonce", Dict),
    Method = Request#request.method,
    URIstr = sipurl:print(Request#request.uri),
    Opaque = case dict:find("opaque", Dict) of
		 {ok, Opaque1} ->
		     Opaque1;
		 error ->
		     ""
	     end,
    User = "2001",
    Password = "testX",
    
    Resp = sipauth:get_response(Nonce, Method, URIstr, User, Password, Realm),
    Resp_str = print_auth_response("Digest", User, Realm, URIstr, Resp, Nonce, Opaque, "MD5"),
    add_authorization2(Request, R, [Resp_str|Headers]).

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

