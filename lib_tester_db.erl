-module (lib_tester_db).

-export ([init_tables/0, delete_tables/0]).
-export ([init_tester/1, least_loaded/0]).
-export ([increment_load/2, decrement_load/2]).
-export ([get_requests/1]).

-record (state, {
    srv_name,       % atom()
    load,           % int()
    requests=[]}).  % [int()]

init_tables() ->
    ok = mnesia:wait_for_tables([state], 60000).

delete_tables() ->
    mnesia:clear_table(state).

init_tester(Name) ->
    case mnesia:dirty_read(state, Name) of
        [] ->
            mnesia:dirty_write(#state{srv_name=Name, load=0});
        [_] ->
            ok
    end.

least_loaded() ->
    Name = mnesia:dirty_first(state),
    least_loaded(mnesia:dirty_next(state, Name), Name).

least_loaded('$end_of_table', Acc) ->
    Acc;
least_loaded(Next, Acc) ->
    Next1 = mnesia:dirty_next(state, Next),
    [#state{load=AccLoad}] = mnesia:dirty_read(state, Acc),
    [#state{load=NextLoad}] = mnesia:dirty_read(state, Next),
    if
        AccLoad > NextLoad -> least_loaded(Next1, Next);
        AccLoad =< NextLoad -> least_loaded(Next1, Acc)
    end.

increment_load(Name, K) ->
    [#state{load=Load, requests=Requests}] = mnesia:dirty_read(state, Name),
    mnesia:dirty_write(#state{srv_name=Name, load=Load+1, requests=[K|Requests]}).

decrement_load(Name, K) ->
    [#state{load=Load, requests=Requests}] = mnesia:dirty_read(state, Name),
    mnesia:dirty_write(#state{srv_name=Name, load=Load-1, requests=lists:delete(K, Requests)}).

get_requests(Name) ->
    [#state{requests=Requests}] = mnesia:dirty_read(state, Name),
    Requests.
