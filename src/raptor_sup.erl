-module(raptor_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(SharedLib) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, SharedLib).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(SharedLib) ->
    {ok, { {one_for_one, 5, 10}, [{
					raptor,
					{raptor, start_link, [SharedLib]},
					permanent,
					2000,
					worker,
					[raptor]
				}]} }.

