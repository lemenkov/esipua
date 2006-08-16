-module(make).

-export([doc/0, doc/1]).

doc() ->
    doc("doc").

doc(DocPath) ->
    {ok, Makefiles} = file:consult("Emakefile"),
    Files = lists:map(fun({F, _Opts}) -> F end, Makefiles),
    edoc:run("", Files, [{dir,DocPath}]).
