-module(siptest).

-behaviour(sipcall).

-include("sdp.hrl").

%% api
-export([run/0, register/0, unregister/0]).

%% sipcall behaviour
-export([init/1]).

-record(state, {}).

run() ->
    [From] = contact:parse(["From Name <sip:2001@skinner.hem.za.org>"]),
    [To] = contact:parse(["To Name <sip:2002@skinner.hem.za.org>"]),

    {ok, Body} = create_sdp_body(siphost:myip(), 12345),

    {ok, Request} = sipcall:build_invite(From, To, Body),
    
    {ok, Call} = sipcall:start_link(?MODULE, [], []),
    ok = sipcall:send_invite(Call, Request),
    {ok, Call}.

create_sdp_body(Localip, Localport) when is_list(Localip),
					 is_integer(Localport) ->
    Seconds = integer_to_list(yate_util:seconds()),
    Origin = #sdp_origin{username="-", session_id=Seconds, version=Seconds,
			 network_type='IN', address_type='IP4',
			 address=Localip},
    Connection = #sdp_connection{network_type='IN', address_type='IP4',
				 address = Localip},
    Media = #sdp_media{media=audio, port=Localport, transport="RTP/AVP",
		       fmts=[8, 0], connection=Connection},
    Sdp = #sdp{origin=Origin, session_name="Yxa", media=[Media]},
    Body = list_to_binary(lists:flatten(sdp:print(Sdp))),
    {ok, Body}.

register() ->
    Aor = get_aor(),

%%     {ok, Request} = sipregister:build_register(Aor),
%%     {ok, Call} = sipregister:start_link(Request),

    ok = register_server:register_aor(Aor),
    ok.

unregister() ->
    Aor = get_aor(),

    ok = register_server:unregister_aor(Aor),
    ok.

get_aor() ->
    "sip:2001@skinner.hem.za.org".
%%      "sip:2001@mulder.hem.za.org".
%%    "sip:105110603@phonzo.com".

%%
%% sipcall callbacks
%%
init([]) ->
    {ok, #state{}}.

