-module(jpetdemo).

%% API
-export([start/0]).

%% API
%%

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(jpetdemo).
