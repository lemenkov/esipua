%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-module(yate_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% api:s
-export([start/0]).

%% application callbacks
start(normal, []) ->
    start().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: start
%% Descrip.: Start and configure the yaws web server application
%% Returns : ok
%%--------------------------------------------------------------------
start() ->
    yate_sup:start_link().
