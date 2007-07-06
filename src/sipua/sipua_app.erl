%%%-------------------------------------------------------------------
%%% File    : sipua.erl
%%% Author  : Mikael Magnusson <mikael@skinner.hem.za.org>
%%% Description : 
%%%
%%% Created : 30 Oct 2006 by Mikael Magnusson <mikael@skinner.hem.za.org>
%%%-------------------------------------------------------------------
-module(sipua_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([make/0]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, []) ->
    case sipua_sup:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

make() ->
    Modules = [
		"sipua_app",
		"sipua_sup",
		"callregister",
		"register_server",
		"register_sup",
		"sdp",
		"sipcall_bye",
		"sipcall",
		"siphelper",
		"sipregister",
		"siptest"
	    ],

    Prefix = "../../../src/sipua/",
    Files = lists:map(fun(File) -> Prefix ++ File end, Modules),

    make:files(Files,
	       [load,
		{i, "../../../include"},
		{i, "/usr/lib/yxa/include"},
		{outdir, "../../src/sipua"},
		debug_info]).

%%====================================================================
%% Internal functions
%%====================================================================
