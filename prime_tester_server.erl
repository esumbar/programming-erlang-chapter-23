-module (prime_tester_server).
-behaviour (gen_server).

-export ([start_link/1, is_prime/2, is_prime_async/2]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include ("state.hrl").

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

is_prime(Name, K) ->
    gen_server:call(Name, {is_prime, K}, 20000).

is_prime_async(Name, K) ->
    gen_server:cast(Name, {is_prime, K}).

init([Name]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [Name]),
    {atomic, #state{requests=Requests}} = mnesia:transaction(
        fun() ->
            case mnesia:read({state, Name}) of
                [] ->
                    NewTester = #state{srv_name=Name},
                    mnesia:write(NewTester),
                    NewTester;
                [OldTester] ->
                    OldTester
            end
        end),
    lists:foreach(fun(K) -> is_prime_async(self(), K) end, lists:reverse(Requests)),
    {ok, {Name, 0}}.

handle_call({is_prime, K}, _From, {Name, N}) ->
    {reply, lib_primes:is_prime(K), {Name, N+1}}.

handle_cast({is_prime, K}, {Name, N}) ->
    load_balancer:print_result_async(Name, K, lib_primes:is_prime(K)),
    {noreply, {Name, N+1}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {Name, _N}) ->
    io:format("~p stopping~n", [Name]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
