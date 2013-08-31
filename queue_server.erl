-module (queue_server).
-behaviour (gen_server).

-export ([start_link/0, add_tester/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_tester(Name) ->
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
