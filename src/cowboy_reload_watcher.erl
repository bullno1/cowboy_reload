-module(cowboy_reload_watcher).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2, format_status/2]).

-record(state, {
	monitor,
	watch_path,
	delay = 0
}).

start_link(WatchPath, RestartDelay) -> gen_server:start_link(?MODULE, {WatchPath, RestartDelay}, []).

% gen_server

init({WatchPath, Delay}) ->
	Cmd = "inotifywait -r -m -q -e close_write,moved_to --format \"%w%f\" " ++ WatchPath,
	{ok, Pid} = gen_os_proc:start_link(Cmd, [{proc_opts, [{hibernate_timeout, 1000}]}]),
	{ok, #state{delay = Delay, watch_path = filename:absname(WatchPath), monitor = Pid}}.

terminate(_Reason, _State) -> ok.

handle_call(_Req, _From, State) -> {stop, unexpected, State}.

handle_cast(_Req, State) -> {stop, unexpected, State}.

handle_info({line, Monitor, Path}, #state{delay = Delay, monitor = Monitor, watch_path = WatchPath} = State) ->
	Absname = filename:absname(Path),
	case string:substr(Absname, 1, length(WatchPath)) of
		WatchPath ->
			RelName = string:substr(Absname, length(WatchPath) + 1),
			timer:apply_after(Delay, cowboy_reload, reload, [RelName]);
		_ -> ignore
	end,
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) ->
	FormattedState = lists:zip(record_info(fields, state), tl(tuple_to_list(State))),
	[{data, [{"State", FormattedState}]}].
