-module (load_balancer).
-behaviour (gen_server).

-export ([start_link/0, dispatch_test_async/1, unload_tester_async/2,
    print_result_async/3, init_tester_async/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

dispatch_test_async(K) ->
    gen_server:cast(?MODULE, {dispatch_test, K}).

unload_tester_async(Name, K) ->
    gen_server:cast(?MODULE, {unload_tester, Name, K}).
print_result_async(Name, K, Result) ->
    gen_server:cast(?MODULE, {print_result, Name, K, Result}).
init_tester_async(Name) ->
    gen_server:cast(?MODULE, {init_tester, Name}).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    lib_tester_db:init_tables("db_tables/tester_table"),
    {ok, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({dispatch_test, K}, N) ->
    Name = lib_tester_db:least_loaded(),
    prime_tester_server:is_prime_async(Name, K),
    lib_tester_db:increment_load(Name, K),
    {noreply, N+1};
handle_cast({unload_tester, Name, K}, N) ->
    lib_tester_db:decrement_load(Name, K),
    {noreply, N+1};
handle_cast({print_result, Name, K, Result}, N) ->
    io:format("From: ~p, is prime: ~p, result: ~p~n", [Name, K, Result]),
    {noreply, N+1};
handle_cast({init_tester, Name}, N) ->
    lib_tester_db:init_tester(Name),
    {noreply, N+1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    lib_tester_db:delete_tables(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
