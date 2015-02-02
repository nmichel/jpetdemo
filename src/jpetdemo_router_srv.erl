-module(jpetdemo_router_srv).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {by_pattern}).

init(Options) ->
    {ok, #state{by_pattern = dict:new()}}.

handle_call({register, Tgt, Pattern}, _From, State) ->
    BP = State#state.by_pattern,
    BP2 = 
        case dict:find(Pattern, BP) of
            error ->
                EPM = ejpet:compile(Pattern),
                dict:store(Pattern, {EPM, [{Tgt}]}, BP);
            {ok, {EPM, Tgts}} ->
                case lists:keyfind(Tgt, 1, Tgts) of
                    false ->
                        dict:store(Pattern, {EPM, [{Tgt}| Tgts]}, BP);
                    _ ->
                        BP
                end
        end,
    {reply, ok, State#state{by_pattern = BP2}};
handle_call({unregister, Tgt, all}, _From, State) ->
    BP = State#state.by_pattern,
    BP2 = dict:map(fun(Pattern, {EPM, Tgts}) ->
                           {EPM, lists:keydelete(Tgt, 1, Tgts)}
                   end, BP),
    BP3 = dict:filter(fun(_Pattern, {_EPM, []}) ->
                             false;
                         (_Pattern, {_EPM, _Tgts}) ->
                             true
                      end, BP2),
    {reply, ok, State#state{by_pattern = BP3}};
handle_call({unregister, Tgt, Pattern}, _From, State) ->
    BP = State#state.by_pattern,
    BP2 = 
        case dict:find(Pattern, BP) of
            error ->
                BP;
            {ok, {EPM, Tgts}} ->
                dict:store(Pattern, {EPM, lists:keydelete(Tgt, 1, Tgts)}, BP)
        end,
    {reply, ok, State#state{by_pattern = BP2}};
handle_call({route, Emitter, Node}, _From, State) ->
    dict:fold(fun(Pattern, {EPM, Tgts}, Acc) ->
                      case ejpet:run(Node, EPM) of 
                          {true, Captures} ->
                              lists:map(fun({Tgt}) ->
                                                Tgt ! {Emitter, Node, Captures}
                                        end, Tgts);
                          {false, _} ->
                              ok
                      end,
                      Acc
              end, [], State#state.by_pattern),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {stop, unknown_request, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.
