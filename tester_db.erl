-module (tester_db).

-export ([init_tables/1, delete_tables/0]).
-export ([init_tester/1, least_loaded_tester/0]).
-export ([increment_load/2, decrement_load/2]).
-export ([get_tester_requests/1]).

-record (state, {
    srv_name,       % atom()
    load,           % int()
    requests=[]}).  % [int()]

init_tables(_FileName) ->
    ets:new(tester_db, [named_table, {keypos, #state.srv_name}]).

delete_tables() ->
    ets:delete(tester_db).

init_tester(Name) ->
    case ets:member(tester_db, Name) of
        true ->
            true;
        false ->
            ets:insert(tester_db, #state{srv_name=Name, load=0})
    end.

least_loaded_tester() ->
    [Acc0] = ets:lookup(tester_db, ets:first(tester_db)),
    #state{srv_name=Name} = ets:foldl(
        fun(#state{load=LoadElem} = Elem, #state{load=LoadAccIn}) when LoadElem < LoadAccIn ->
                Elem;
            (_, AccIn) ->
                AccIn
        end,
        Acc0,
        tester_db),
    Name.

increment_load(Name, K) ->
    [#state{load=Load, requests=Requests}] = ets:lookup(tester_db, Name),
    ets:update_element(tester_db, Name,
        [{#state.load, Load+1}, {#state.requests, [K|Requests]}]).

decrement_load(Name, K) ->
    [#state{load=Load, requests=Requests}] = ets:lookup(tester_db, Name),
    ets:update_element(tester_db, Name,
        [{#state.load, Load-1}, {#state.requests, lists:delete(K, Requests)}]).

get_tester_requests(Name) ->
    [#state{requests=Requests}] = ets:lookup(tester_db, Name),
    Requests.
