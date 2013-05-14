-module(raptor).

-behaviour(gen_server).

%% gen_server interface
-export([start_link/1, start/1]).

%% gen_server callbacks
-export ([init/1,
	      handle_call/3,
	      handle_cast/2,
	      handle_info/2,
	      code_change/3,
	      terminate/2]).

%% API functions
-export([parse_uri/1, parse_uri/2, test/0]).

%% server state
-record(state, {port}).

%% ===================================================================
%% gen_server interface 
%% ===================================================================
start_link(SharedLib) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, SharedLib, []).

start(SharedLib) ->
	gen_server:start({local, ?MODULE}, ?MODULE, SharedLib, []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init(SharedLib) ->
	case load_driver(SharedLib) of
		{ok, Port}   -> {ok, #state{port = Port}};
		{error, Msg} -> {stop, Msg};
		_            -> {stop, failed_to_load_driver}
	end.

load_driver(SharedLib) ->
	Result = case erl_ddll:load_driver("../priv", SharedLib) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, ErrorDesc} -> {error, erl_ddll:format_error(ErrorDesc)}
	end,
	case Result of
		ok ->
			process_flag(trap_exit,true),
			case open_port({spawn, SharedLib},[]) of
				P when is_port(P) -> {ok, P};
				Error -> {error, Error}
			end;
		{error, Reason} -> {error, Reason}
	end.

handle_cast(_Msg, State) ->
	{noreply, State}.

%% code_change not working, implementing stub
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info({'EXIT', _Port, Reason}, State) ->
	{stop, {port_terminated, Reason}, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate({port_terminated, _Reason}, _State) ->
	ok;
terminate(_Reason, #state{port = Port} = _State) ->
	port_close(Port).

handle_call(Msg, _From, #state{port = Port} = State) ->
	port_command(Port, encode(Msg)),
	case collect_response() of
		{error, Reason} ->
			{stop, Reason, State};
		{response, Response} ->
			{reply, Response, State};
		timeout ->
			{stop, port_timeout, State}
	end.

collect_response() ->
	collect_response([]).

collect_response(Acc) ->
	receive
		{error, Reason}->
			{error, Reason};
		ok ->
			{response, lists:reverse(Acc)};
		Data ->
			collect_response([Data|Acc])
	end.

%% ===================================================================
%% API functions
%% ===================================================================
test() ->
	call_server({test}).

parse_uri(Uri) ->
	call_server({parse_uri,Uri}).

parse_uri(Uri,Format) ->
	call_server({parse_uri, Uri, Format}).

call_server(Msg) ->
	gen_server:call(?MODULE, Msg, get_timeout()).

get_timeout() ->
	{ok, Value} = application:get_env(raptor, timeout),
	Value.

encode({test}) -> [1];
encode({parse_uri, Uri}) -> encode({parse_uri, Uri, default});
encode({parse_uri, Uri, Format}) -> [2,encode_format(Format),Uri].

encode_format(In) ->
	case In of
		default      -> 0; % default (RDF/XML) 
		rdfxml       -> 1; % RDF/XML (default)
		ntriples     -> 2; % N-Triples
		turtle       -> 3; % Turtle Terse RDF Triple Language
		trig         -> 4; % TriG - Turtle with Named Graphs
		rss_tag_soup -> 5; % RSS Tag Soup
		grddl        -> 6; % Gleaning Resource Descriptions from Dialects of Languages
		guess        -> 7; % Pick the parser to use using content type and URI
		rdfa         -> 8; % RDF/A via librdfa
		nquads       -> 9 % N-Quadsd
	end.
