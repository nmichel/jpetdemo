-module(jpetdemo_router).

%% API
-export([start_link/0,
         register/3,
         unregister/3,
         unregister/2,
         route/2]).

-export([test/0]).

%% API
%%
start_link() ->
    gen_server:start_link(jpetdemo_router_srv, [], []).

register(Srv, Tgt, Pattern) ->
    gen_server:call(Srv, {register, Tgt, Pattern}).

unregister(Srv, Tgt, Pattern) ->
    gen_server:call(Srv, {unregister, Tgt, Pattern}).

unregister(Srv, Tgt) ->
    gen_server:call(Srv, {unregister, Tgt, all}).

route(Srv, Node) ->
    gen_server:call(Srv, {route, Node}).

%% Test

test() ->
    {ok, Srv} = jpetdemo_router:start([]),
    jpetdemo_router:register(Srv, self(), "42"),
    jpetdemo_router:register(Srv, self(), "42"),
    %% jpetdemo_router:unregister(Srv, self(), "true"),
    %% jpetdemo_router:unregister(Srv, self()),
    jpetdemo_router:route(Srv, jsx:decode(<<"42">>)),
    receive
        Node ->
            Node
    after
        0 ->
            ok
    end.
