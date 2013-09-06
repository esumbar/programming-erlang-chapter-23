-module (prime_tester_supervisor).
-behaviour (supervisor).

-export ([start/0, start_link/0]).
-export ([init/1]).

start() ->
    spawn(fun() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
    end).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_all, 3, 10},
            [{tag3,
                {load_balancer, start_link, []},
                permanent,
                10000,
                worker,
                [load_balancer]},
            {tag14,
                {prime_tester_server_supervisor, start_link, []},
                permanent,
                10000,
                supervisor,
                [prime_tester_server_supervisor]}]}}.
