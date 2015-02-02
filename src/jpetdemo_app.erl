-module(jpetdemo_app).
-behaviour(application).

-export([start/2,
         stop/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_PARAMS(I, Params, Type), {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

start(_StartType, _StartArgs) ->
    {ok, SupRef} = jpetdemo_sup:start_link(),
    {ok, RouterRef} = supervisor:start_child(SupRef, ?CHILD(jpetdemo_router, worker)),
    {ok, CntrlrRef} = supervisor:start_child(SupRef, ?CHILD_PARAMS(jpetdemo_controller_srv, [[RouterRef]], worker)),

    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", cowboy_static, {file, "./priv/index.html"}},
                                             {"/websocket", jpetdemo_ws_handler, [{RouterRef}]},
                                             {"/static/[...]", cowboy_static, {dir, "./priv/static"}}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 9000}],
                                [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
    ok.
