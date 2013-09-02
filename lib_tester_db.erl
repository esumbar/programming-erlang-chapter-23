-module (lib_tester_db).

-export ([init_tables/1, delete_tables/0]).
-export ([init_tester/1, least_loaded/0]).
-export ([increment_load/2, decrement_load/2]).
-export ([get_requests/1]).

-record (state, {
    srv_name,       % atom()
    load,           % int()
    requests=[]}).  % [int()]

init_tables(_FileName) ->
    ets:new(ets_tester_db, [named_table, {keypos, #state.srv_name}]).

delete_tables() ->
    ets:delete(ets_tester_db).

init_tester(Name) ->
    case ets:member(ets_tester_db, Name) of
        true ->
            true;
        false ->
            ets:insert(ets_tester_db, #state{srv_name=Name, load=0})
    end.

least_loaded() ->
    [Acc0] = ets:lookup(ets_tester_db, ets:first(ets_tester_db)),
    #state{srv_name=Name} = ets:foldl(
        fun(#state{load=LoadElem} = Elem, #state{load=LoadAccIn}) when LoadElem < LoadAccIn ->
                Elem;
            (_, AccIn) ->
                AccIn
        end,
        Acc0,
        ets_tester_db),
    Name.

increment_load(Name, K) ->
    [#state{load=Load, requests=Requests}] = ets:lookup(ets_tester_db, Name),
    ets:update_element(ets_tester_db, Name,
        [{#state.load, Load+1}, {#state.requests, [K|Requests]}]).

decrement_load(Name, K) ->
    [#state{load=Load, requests=Requests}] = ets:lookup(ets_tester_db, Name),
    ets:update_element(ets_tester_db, Name,
        [{#state.load, Load-1}, {#state.requests, lists:delete(K, Requests)}]).

get_requests(Name) ->
    [#state{requests=Requests}] = ets:lookup(ets_tester_db, Name),
    Requests.
