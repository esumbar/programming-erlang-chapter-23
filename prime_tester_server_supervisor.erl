-module (prime_tester_server_supervisor).
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
            {one_for_one, 3, 10},
            [{tag4,
                {prime_tester_server, start_link, [prime_tester1]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag5,
                {prime_tester_server, start_link, [prime_tester2]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag6,
                {prime_tester_server, start_link, [prime_tester3]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag7,
                {prime_tester_server, start_link, [prime_tester4]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag8,
                {prime_tester_server, start_link, [prime_tester5]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag9,
                {prime_tester_server, start_link, [prime_tester6]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag10,
                {prime_tester_server, start_link, [prime_tester7]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag11,
                {prime_tester_server, start_link, [prime_tester8]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag12,
                {prime_tester_server, start_link, [prime_tester9]},
                permanent,
                10000,
                worker,
                [prime_tester_server]},
            {tag13,
                {prime_tester_server, start_link, [prime_tester10]},
                permanent,
                10000,
                worker,
                [prime_tester_server]}]}}.
