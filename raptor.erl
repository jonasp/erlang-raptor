-module(raptor).
-export([start/0, stop/0, init/1]).
-export([parse_uri/1, test/0]).

start(SharedLib) ->
    case erl_ddll:load_driver(".", SharedLib) of
	ok -> ok;
	{error, already_loaded} -> ok;
	_ -> exit({error, could_not_load_driver})
    end,
    spawn(?MODULE, init, [SharedLib]).

start() ->
	start("raptor_drv"),
	ok.

init(SharedLib) ->
    register(raptor, self()),
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

stop() ->
    raptor ! stop.

test() ->
	call_port({test}).
parse_uri(Uri) ->
	call_port({parse_uri,Uri}).

call_port(Msg) ->
    raptor ! {call, self(), Msg},
    receive
	{raptor, Result} ->
	    Result
    end.

loop(Port) ->
	receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
		result([], Caller),
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("~p ~n", [Reason]),
	    exit(port_terminated)
    end.

result(Statements, Caller) ->
	receive
		ok ->
			Caller ! {raptor, lists:reverse(Statements)};
		Data ->
			result([Data|Statements],Caller)
	end.


encode({test}) -> [1];
encode({parse_uri,Uri}) -> [2,Uri].
