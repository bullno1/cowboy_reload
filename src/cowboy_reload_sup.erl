-module(cowboy_reload_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	%{Name, {Module, StartFunc, Args}, permanent, 5000, supervisor, [Module]}
	{ok, WatchPaths} = application:get_env(cowboy_reload, watch_paths),
	WatcherSpecs = [watcher_spec(Path) || Path <- WatchPaths],
	ChildSpecs = [ranch_spec(), gulp_spec() | WatcherSpecs],
	{ok, { {one_for_one, 5, 60}, ChildSpecs}}.

ranch_spec() ->
	Routes = [
		{"/livereload.js", cowboy_static, {priv_file, cowboy_reload, "livereload.js"}},
		{"/livereload", cowboy_reload_ws, []}
	],
	Dispatch = cowboy_router:compile([
		{'_', Routes}
	]),
	{ok, Port} = application:get_env(cowboy_reload, port),
	TransOpts = [{port, Port}],
	ProtoOpts = [{env, [{dispatch, Dispatch}]}],
	ranch:child_spec(cowboy_reload_http, 100, ranch_tcp, TransOpts, cowboy_protocol, ProtoOpts).

watcher_spec(Path) ->
	{{cowboy_reload_watcher, Path},
	 {cowboy_reload_watcher, start_link, [Path]},
	 permanent, 1000, worker, [cowboy_reload_watcher]}.

gulp_spec() ->
	{cowboy_reload_gulp,
	 {cowboy_reload_gulp, start_link, []},
	 permanent, 1000, worker, [cowboy_reload_gulp]}.
