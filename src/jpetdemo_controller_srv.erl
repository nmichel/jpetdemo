-module(jpetdemo_controller_srv).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {router}).

start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

init([Router]) ->
    jpetdemo_router:register(Router, self(), "{\"ctrl\": {\"subscribe\": [\"room\", (?<sub_room>_)]}}"),
    jpetdemo_router:register(Router, self(), "{\"ctrl\": {\"unsubscribe\": [\"room\", (?<unsub_room>_)]}}"),
    jpetdemo_router:register(Router, self(), "{\"ctrl\": {\"subscribe\": [\"topic\", (?<sub_topic>_)]}}"),
    jpetdemo_router:register(Router, self(), "{\"ctrl\": {\"unsubscribe\": [\"topic\", (?<unsub_topic>_)]}}"),
    {ok, #state{router = Router}}.

handle_call(_Request, _From, State) ->
    {stop, unknown_request, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info = {From, Node, [{"sub_room", Room}]}, State) ->
    jpetdemo_router:register(State#state.router, From, "{\"room\":\"" ++ binary_to_list(Room) ++ "\"}"),
    {noreply, State};
handle_info(Info = {From, Node, [{"unsub_room", Room}]}, State) ->
    jpetdemo_router:unregister(State#state.router, From, "{\"room\":\"" ++ binary_to_list(Room) ++ "\"}"),
    {noreply, State};
handle_info(Info = {From, Node, [{"sub_topic", Topic}]}, State) ->
    jpetdemo_router:register(State#state.router, From, "{\"msg\":#\"#" ++ binary_to_list(Topic) ++ "\"}"),
    {noreply, State};
handle_info(Info = {From, Node, [{"unsub_topic", Topic}]}, State) ->
    jpetdemo_router:unregister(State#state.router, From, "{\"msg\":#\"#" ++ binary_to_list(Topic) ++ "\"}"),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.
