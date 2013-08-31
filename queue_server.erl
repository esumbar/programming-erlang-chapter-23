-module (queue_server).
-behaviour (gen_server).

-export ([start_link/0, enqueue_async/1, free_tester_async/1,
    print_result_async/3, add_tester_async/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enqueue_async(K) ->
    gen_server:cast(?MODULE, {enqueue, K}).
free_tester_async(Name) ->
    gen_server:cast(?MODULE, {free_tester, Name}).
print_result_async(Name, K, Result) ->
    gen_server:cast(?MODULE, {print_result, Name, K, Result}).
add_tester_async(Name) ->
    gen_server:cast(?MODULE, {add_tester, Name}).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    RequestQueue = queue:new(),
    TesterStatus = ets:new(tester_status, [named_table]),
    {ok, {RequestQueue, TesterStatus}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({enqueue, K}, {RequestQueue, TesterStatus}) ->
    RequestQueue1 = queue:in(K, RequestQueue),
    NewRequestQueue = case ets:match(TesterStatus, {'$1', free}) of
        [] ->
            RequestQueue1;
        [H|_] ->
            FreeServer = hd(H),
            {{value, K1}, RequestQueue2} = queue:out(RequestQueue1),
            prime_tester_server:is_prime_async(FreeServer, K1),
            ets:update_element(TesterStatus, FreeServer, {2, busy}),
            RequestQueue2
    end,
    {noreply, {NewRequestQueue, TesterStatus}};
handle_cast({free_tester, Name}, {RequestQueue, TesterStatus}) ->
    NewRequestQueue = case queue:out(RequestQueue) of
        {empty, _} ->
            ets:update_element(TesterStatus, Name, {2, free}),
            RequestQueue;
        {{value, K}, RequestQueue1} ->
            prime_tester_server:is_prime_async(Name, K),
            RequestQueue1
    end,
    {noreply, {NewRequestQueue, TesterStatus}};
handle_cast({print_result, Name, K, Result}, State) ->
    io:format("From: ~p, is prime: ~p, result: ~p~n", [Name, K, Result]),
    {noreply, State};
handle_cast({add_tester, Name}, {RequestQueue, TesterStatus}) ->
    ets:insert(TesterStatus, {Name, free}),
    {noreply, {RequestQueue, TesterStatus}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {_, TesterStatus}) ->
    io:format("~p stopping~n", [?MODULE]),
    ets:delete(TesterStatus),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
