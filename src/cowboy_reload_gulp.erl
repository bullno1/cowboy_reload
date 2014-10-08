-module(cowboy_reload_gulp).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2, format_status/2]).

-record(state, {
	gulp
}).

start_link() ->
	case os:find_executable("gulp") of
		Path when is_list(Path) ->
			gen_server:start_link({local, ?MODULE}, ?MODULE, Path, []);
		false -> ignore
	end.

% gen_server

init(GulpPath) ->
	Cmd = GulpPath ++ " watch",
	Opts = [
		{proc_opts, [{hibernate_timeout, 1000}]},
		{port_opts, [stderr_to_stdout]}
	],
	{ok, Pid} = gen_os_proc:start_link(Cmd, Opts),
	{ok, #state{gulp = Pid}}.

terminate(_Reason, _State) -> ok.

handle_call(_Req, _From, State) -> {stop, unexpected, State}.

handle_cast(_Req, State) -> {stop, unexpected, State}.

handle_info({line, Gulp, Line}, #state{gulp = Gulp} = State) ->
	error_logger:info_report([{gulp, Line}]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) ->
	FormattedState = lists:zip(record_info(fields, state), tl(tuple_to_list(State))),
	[{data, [{"State", FormattedState}]}].
