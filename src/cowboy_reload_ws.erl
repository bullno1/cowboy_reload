-module(cowboy_reload_ws).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_info/3, websocket_handle/3, websocket_terminate/3]).

-define(HELLO_REPLY, <<
	"{"
		"\"command\":\"hello\","
		"\"protocols\":["
			"\"http://livereload.com/protocols/official-6\","
			"\"http://livereload.com/protocols/official-7\","
			"\"http://livereload.com/protocols/connection-check-1\""
		"],"
		"\"serverName\":\"cowboy_reload\""
	"}"
>>).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
	gproc:reg({p, l, {cowboy_reload, ws}}),
	{ok, cowboy_req:compact(Req), []}.

websocket_terminate(_, _, _) -> gproc:goodbye().

websocket_info({reload, Path}, Req, State) ->
	Cmd = jsx:encode(#{
		<<"command">> => <<"reload">>,
		<<"path">> => Path,
		<<"liveCSS">> => true
	}),
	{reply, {text, Cmd}, Req, State, hibernate};

websocket_info({alert, Msg}, Req, State) ->
	Cmd = jsx:encode(#{
		<<"command">> => alert,
		<<"message">> => Msg
	}),
	{reply, {text, Cmd}, Req, State, hibernate}.

websocket_handle({text, Data}, Req, State) ->
	Msg = jsx:decode(Data),
	Cmd = proplists:get_value(<<"command">>, Msg),
	handle_cmd(Cmd, Msg, Req, State);

websocket_handle(Frame, Req, State) ->
	error_logger:warning_report([{dropped_ws_frame, Frame}]),
	{ok, Req, State, hibernate}.

% private

handle_cmd(<<"hello">>, _Msg, Req, State) ->
	{reply, {text, ?HELLO_REPLY}, Req, State, hibernate};

handle_cmd(<<"info">>, _Msg, Req, State) ->
	{ok, Req, State, hibernate};

handle_cmd(_, Msg, Req, State) ->
	error_logger:warning_report([{unhandled_msg, Msg}]),
	{ok, Req, State, hibernate}.
