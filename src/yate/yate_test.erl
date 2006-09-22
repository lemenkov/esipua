-module(yate_test).

-export([test/0]).

test() ->
    %% Application installs a handler for a "test" handler with priority 50
    %%test_command("%%>install:50:test"),

    %% Engine installs the handler and acknowledges it
    test_command("%%<install:50:test:true"),

    %% Application installs a handler for the "engine.timer" handler with default priority
    %%test_command("%%>install::engine.timer"),

    %% Engine installs the handler and acknowledges it
    test_command("%%<install:100:engine.timer:true"),

    %% Application emits a private "app.job" message; notice how the '%' and ':' characters were escaped
    test_command("%%>message:myapp55251:1095112794:app.job::job=cleanup:done=75%%:path=/bin%z/usr/bin"),

    %% Engine just dispatched an "engine.timer" message
    test_command("%%>message:234479208:1095112795:engine.timer::time=1095112795"),

    %% Engine gets back the answer to the "app.job" message with a "Restart required" returned text value
    test_command("%%<message:myapp55251:true:app.job:Restart required:path=/bin%z/usr/bin%z/usr/local/bin"),

    %% Application ignores the timer message
    test_command("%%<message:234479208:false:engine.timer::time=1095112795"),

    %% Application uninstalls the "test" handler
    %%test_command("%%>uninstall:test"),

    %% Engine removes the handler and acknowledges it
    test_command("%%<uninstall:50:test:true"),

    %% Engine just dispatched another "engine.timer" message
    test_command("%%>message:234479288:1095112796:engine.timer::time=1095112796"),

    %% Application modifies the timer message but lets it flow further
    test_command("%%<message:234479288:false:engine.timer::time=1095112796:extra=yes"),

    %% Engine just dispatched another "engine.timer" message
    test_command("%%>message:234479244:1095112797:engine.timer::time=1095112797"),
    
    %% Application suddenly exits without acknowledging previous message.
    %% The engine releases the message 234479244 (as if "false" was returned) and
    %% uninstalls the remaining "engine.timer" handler
    my_trace("Test ok"),
    ok.

test_command(Data) ->
    case yate_decode:decode_command(Data) of
	{ok, _Dir, _Cmd} ->
%% 	    my_trace("OK: ", [Cmd]),
	    ok;
	{error, Reason} ->
	    my_trace("Decode error", [Reason])
    end.


my_trace(Msg, Args) ->
    io:format("~p ~p ~n", [Msg, Args]).

my_trace(Msg) ->
    io:format("~p ~n", [Msg]).
