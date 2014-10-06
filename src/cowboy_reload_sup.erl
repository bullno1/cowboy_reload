-module(cowboy_reload_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	%{Name, {Module, StartFunc, Args}, permanent, 5000, supervisor, [Module]}
	ChildSpecs = [ranch_spec(), watcher_spec()],
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

watcher_spec() ->
	{cowboy_reload_watcher,
	 {cowboy_reload_watcher, start_link, ["priv/www"]},
	 permanent, 1000, supervisor, [cowboy_reload_watcher]}.
