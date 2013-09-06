-module (load_balancer).
-behaviour (gen_server).

-export ([start_link/0]).
-export ([dispatch_test_async/1]).
-export ([print_result_async/3]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include ("state.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

dispatch_test_async(K) ->
    gen_server:cast(?MODULE, {dispatch_test, K}).

print_result_async(Name, K, Result) ->
    gen_server:cast(?MODULE, {print_result, Name, K, Result}).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    ok = mnesia:wait_for_tables([state], 60000),
    {ok, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({dispatch_test, K}, N) ->
    {atomic, ok} = mnesia:transaction(
        fun() ->
            [Acc0] = mnesia:read(state, mnesia:first(state)),
            LeastLoadedTester = mnesia:foldl(
                fun(#state{load=RecLoad} = Rec, #state{load=AccLoad}) when RecLoad < AccLoad ->
                        Rec;
                    (_, Acc) ->
                        Acc
                end,
                Acc0,
                state),
            #state{srv_name=Name, load=Load, requests=Requests} = LeastLoadedTester,
            prime_tester_server:is_prime_async(Name, K),
            mnesia:write(#state{srv_name=Name, load=Load+1, requests=[K|Requests]})
        end),
    {noreply, N+1};
handle_cast({print_result, Name, K, Result}, N) ->
    {atomic, ok} = mnesia:transaction(
        fun() ->
            io:format("From: ~p, is prime: ~p, result: ~p~n", [Name, K, Result]),
            [#state{load=Load, requests=Requests}] = mnesia:read(state, Name),
            mnesia:write(#state{srv_name=Name, load=Load-1, requests=lists:delete(K, Requests)})
        end),
    {noreply, N+1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    mnesia:clear_table(state),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
