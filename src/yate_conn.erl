%%%
%%% @doc       Connection module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(yate_conn).

%% api
-export([connect/3, stop/1, install/2, install/3, uninstall/2, watch/2, unwatch/2, ret/3, ret/4, queue_msg/4, send_msg/3]).

-include("yate.hrl").

%%--------------------------------------------------------------------
%% @spec connect(Host, Port) -> Result
%%           Host = string() | atom() | ip_address()
%%           Port = integer()
%%           Result = {ok, Handle} | {error, Reason}
%% @doc Connect to Yate extmodule
%% @end
%%--------------------------------------------------------------------
connect(Host, Port, Pid) ->
    yate_sup:start_child(Host, Port, Pid).

%%--------------------------------------------------------------------
%% @spec watch(Handle, Name) -> true
%%           Handle = pid()
%%           Name = atom()
%% @doc install message watcher (post-dispatching notifier)
%% @end
%%--------------------------------------------------------------------
watch(Handle, Name) ->
    call(Handle, {watch, Name}).

%%--------------------------------------------------------------------
%% @spec unwatch(Handle, Name) -> true
%%           Handle = pid()
%%           Name = atom()
%% @doc uninstall message watcher (post-dispatching notifier)
%% @end
%%--------------------------------------------------------------------
unwatch(Handle, Name) ->
    call(Handle, {unwatch, Name}).

%%--------------------------------------------------------------------
%% @spec install(Handle, Name) -> ok
%%           Handle = pid()
%%           Name = atom()
%% @doc install message handler
%% @end
%%--------------------------------------------------------------------
install(Handle, Name) ->
    install(Handle, Name, 10).

%%--------------------------------------------------------------------
%% @spec install(Handle, Name, Priority) -> ok | error
%%           Handle = pid()
%%           Name = atom()
%%           Priority = integer()
%% @doc install message handler
%% @end
%%--------------------------------------------------------------------
install(Handle, Name, Priority) ->
    call(Handle, {install, Name, Priority}).


%%--------------------------------------------------------------------
%% @spec uninstall(Handle, Name) -> ok
%%           Handle = pid()
%%           Name = atom()
%% @doc uninstall message handler
%% @end
%%--------------------------------------------------------------------
uninstall(Handle, Name) ->
    call(Handle, {uninstall, Name}).

%%--------------------------------------------------------------------
%% @spec ret(Handle, Cmd, Processed) -> ok
%%           Handle = pid()
%%           Cmd = #command{}
%% @doc answer message
%% @end
%%--------------------------------------------------------------------
ret(Handle, Cmd, Processed) ->
    Header = (Cmd#command.header)#message{processed=Processed},
    gen_server:cast(Handle, {ret, Cmd#command{header=Header}}).

%%--------------------------------------------------------------------
%% @spec ret(Handle, Cmd, Processed, Retval) -> ok
%%           Handle = pid()
%%           Cmd = #command{}
%%           Retval = string()
%% @doc answer message
%% @end
%%--------------------------------------------------------------------
ret(Handle, Cmd, Processed, Retval) ->
    Header = (Cmd#command.header)#message{processed=Processed},
    gen_server:cast(Handle, {ret, Cmd#command{retvalue=Retval,header=Header}}).

%%--------------------------------------------------------------------
%% @spec queue_msg(Handle, Name, Keys) -> ok
%%           Handle = pid()
%%           Name = string()
%%           Keys = dictionary()
%% @doc post message without waiting for answer
%% @end
%%--------------------------------------------------------------------
queue_msg(Handle, Name, Keys, Tag) ->
    gen_server:cast(Handle, {msg, Name, Keys, self(), Tag}).


%%--------------------------------------------------------------------
%% @spec msg(Handle, Name, Keys) -> {ok, Cmd}
%%           Handle = pid()
%%           Name = string()
%%           Keys = dictionary()
%% @doc send message and wait for answer
%% @end
%%--------------------------------------------------------------------
send_msg(Handle, Name, Keys) ->
    gen_server:call(Handle, {msg, Name, Keys}).


%%--------------------------------------------------------------------
%% @spec stop(Handle) -> ok
%%           Handle = pid()
%% @doc close connection
%% @end
%%--------------------------------------------------------------------
stop(Handle) ->
    gen_server:cast(Handle, stop).


%%
%% private functions
%%
call(Handle, Request) ->
    Retvalue = case gen_server:call(Handle, Request) of
	{ok, Retvalue2} ->
	    Retvalue2;
	{ok, Retvalue2, _Cmd} ->
	    Retvalue2
    end,
    case Retvalue of
	true ->
	    ok;
	false ->
	    error
    end.
