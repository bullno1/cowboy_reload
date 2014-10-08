-module(cowboy_reload).
-behaviour(application).
-export([reload/1, alert/1]).
-export([start/2, stop/1]).

-spec reload(string() | binary()) -> ok.
reload(Path) -> send_cmd(reload, Path).

-spec alert(string() | binary()) -> ok.
alert(Msg) -> send_cmd(alert, Msg).

% application

start(_StartType, _StartArgs) -> cowboy_reload_sup:start_link().

stop(_State) -> ok.

% private

send_cmd(CmdType, Param) when is_binary(Param) ->
	gproc:send({p, l, {cowboy_reload, ws}}, {CmdType, Param}),
	ok;

send_cmd(CmdType, Param) when is_list(Param) ->
	gproc:send({p, l, {cowboy_reload, ws}}, {CmdType, list_to_binary(Param)}),
	ok.
