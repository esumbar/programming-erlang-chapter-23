-module (load_balancer).
-behaviour (gen_server).

-export ([start_link/0, dispatch_work_async/1, unload_tester_async/2,
    print_result_async/3, add_tester_async/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

dispatch_work_async(K) ->
    gen_server:cast(?MODULE, {dispatch_work, K}).
unload_tester_async(Name, K) ->
    gen_server:cast(?MODULE, {unload_tester, Name, K}).
print_result_async(Name, K, Result) ->
    gen_server:cast(?MODULE, {print_result, Name, K, Result}).
add_tester_async(Name) ->
    gen_server:cast(?MODULE, {add_tester, Name}).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    TesterLoad = ets:new(tester_load, [named_table]),
    {ok, TesterLoad}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({dispatch_work, K}, TesterLoad) ->
    {LeastLoaded, Load, Requests} = least_loaded(TesterLoad),
    prime_tester_server:is_prime_async(LeastLoaded, K),
    ets:update_element(TesterLoad, LeastLoaded, [{2, Load+1}, {3, [K|Requests]}]),
    {noreply, TesterLoad};
handle_cast({unload_tester, Name, K}, TesterLoad) ->
    [{_, Load, Requests}] = ets:lookup(TesterLoad, Name),
    ets:update_element(TesterLoad, Name, [{2, Load-1}, {3, lists:delete(K, Requests)}]),
    {noreply, TesterLoad};
handle_cast({print_result, Name, K, Result}, State) ->
    io:format("From: ~p, is prime: ~p, result: ~p~n", [Name, K, Result]),
    {noreply, State};
handle_cast({add_tester, Name}, TesterLoad) ->
    %% Data object: server name, load, list of requests
    ets:insert(TesterLoad, {Name, 0, []}),
    {noreply, TesterLoad};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, TesterLoad) ->
    io:format("~p stopping~n", [?MODULE]),
    ets:delete(TesterLoad),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

least_loaded(TesterLoad) ->
    [Acc0] = ets:lookup(TesterLoad, ets:first(TesterLoad)),
    ets:foldl(
        fun({_, Load, _} = Tester, {_, LoadAcc, _}) when Load < LoadAcc -> Tester;
            (_, AccIn) -> AccIn
        end,
        Acc0,
        TesterLoad).
