-module(jpetdemo_app).
-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%% Application callbacks
%% 

start(_StartType, _StartArgs) ->
    jpetdemo_sup:start_link(),
    [{jpetdemo_router, RouterSrv, _, _}] = supervisor:which_children(jpetdemo_sup),

    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", cowboy_static, {priv_file, jpetdemo, "index.html"}},
                                             {"/websocket", jpetdemo_ws_handler, [{RouterSrv}]},
                                             {"/static/[...]", cowboy_static, {priv_dir, jpetdemo, "static"}}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 9000}],
                                [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
    ok.
