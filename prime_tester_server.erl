-module (prime_tester_server).
-behaviour (gen_server).

-export ([start_link/0, is_prime/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

is_prime(N) -> gen_server:call(?MODULE, {is_prime, N}, 20000).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({is_prime, K}, _From, N) ->
    {reply, lib_primes:is_prime(K), N+1}.

handle_cast(_Msg, N) ->
    {noreply, N}.

handle_info(_Info, N) ->
    {noreply, N}.

terminate(_Reason, _N) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) ->
    {ok, N}.
