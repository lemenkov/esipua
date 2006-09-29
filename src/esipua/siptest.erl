-module(siptest).

-behaviour(sipcall).

%% api
-export([run/0, register/0, unregister/0]).

%% sipcall behaviour
-export([init/1]).

-record(state, {}).

run() ->
    [From] = contact:parse(["From Name <sip:2001@skinner.hem.za.org>"]),
    [To] = contact:parse(["To Name <sip:2002@skinner.hem.za.org>"]),

    {ok, Request} = sipcall:build_invite(From, To, <<>>),
    
    {ok, Call} = sipcall:start_link(?MODULE, [], []),
    ok = sipcall:send_invite(Call, Request),
    {ok, Call}.

register() ->
    
    Aor = "sip:2001@skinner.hem.za.org",

%%     {ok, Request} = sipregister:build_register(Aor),
%%     {ok, Call} = sipregister:start_link(Request),

    ok = register_server:register_aor(Aor),
    ok.

unregister() ->
    Aor = "sip:2001@skinner.hem.za.org",

    ok = register_server:unregister_aor(Aor),
    ok.

%%
%% sipcall callbacks
%%
init([]) ->
    {ok, #state{}}.

