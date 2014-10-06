-module(cowboy_reload).
-behaviour(application).
-export([reload/1, alert/1]).
-export([start/2, stop/1]).

reload(Path) -> gproc:send({p, l, {cowboy_reload, ws}}, {reload, Path}), ok.

alert(Msg) -> gproc:send({p, l, {cowboy_reload, ws}}, {alert, Msg}), ok.

% application

start(_StartType, _StartArgs) -> cowboy_reload_sup:start_link().

stop(_State) -> ok.
