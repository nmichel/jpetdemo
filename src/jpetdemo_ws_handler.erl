-module(jpetdemo_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-record(state, {router}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, [{Router}]) ->
    {ok, Req, #state{router = Router}}.

websocket_handle({text, Msg}, Req, State = #state{router = Router}) ->
    jpetdemo_router:route(Router, self(), jsx:decode(Msg)),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({From, Node, Captures}, Req, State) ->
    {reply, {text, jsx:encode(Node)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
