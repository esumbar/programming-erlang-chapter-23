-module (sellaprime_supervisor).
-behaviour (supervisor).

-export ([start/0, start_link/1, start_in_shell_for_testing/0]).
-export ([init/1]).

start() ->
    spawn(fun() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
    end).

start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    gen_event:swap_handler(alarm_handler,
        {alarm_handler, swap},
        {my_alarm_handler, xyz}),
    {ok,
        {
            {one_for_one, 3, 10},
            [{tag1,
                {area_server, start_link, []},
                permanent,
                10000,
                worker,
                [area_server]},
            {tag2,
                {prime_server, start_link, []},
                permanent,
                10000,
                worker,
                [prime_server]},
            {tag3,
                {queue_server, start_link, []},
                permanent,
                10000,
                worker,
                [queue_server]},
            {tag4,
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
