-module(raptor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, SharedLib} = application:get_env(raptor, driver),
	raptor_sup:start_link(SharedLib).

stop(_State) ->
    ok.
