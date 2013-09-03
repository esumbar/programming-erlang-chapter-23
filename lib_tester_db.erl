-module (lib_tester_db).

-export ([init_tables/1, delete_tables/0]).
-export ([init_tester/1, least_loaded/0]).
-export ([increment_load/2, decrement_load/2]).
-export ([get_requests/1]).

-record (state, {
    srv_name,       % atom()
    load,           % int()
    requests=[]}).  % [int()]

init_tables(FileName) ->
    dets:open_file(dets_tester_db,
        [{file, FileName}, {keypos, #state.srv_name}]),
    case dets:info(dets_tester_db, size) of
        0 ->
            ets:new(ets_tester_db, [named_table, {keypos, #state.srv_name}]);
        Size when is_integer(Size), Size > 0 ->
            ets:new(ets_tester_db, [named_table, {keypos, #state.srv_name}]),
            dets:to_ets(dets_tester_db, ets_tester_db)
    end.

delete_tables() ->
    ets:delete(ets_tester_db),
    dets:close(dets_tester_db).

init_tester(Name) ->
    case ets:member(ets_tester_db, Name) of
        false ->
            ets:insert(ets_tester_db, #state{srv_name=Name, load=0}),
            dets:insert(dets_tester_db, #state{srv_name=Name, load=0});
        true ->
            true
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
        [{#state.load, Load+1}, {#state.requests, [K|Requests]}]),
    dets:insert(dets_tester_db,
        #state{srv_name=Name, load=Load+1, requests=[K|Requests]}).

decrement_load(Name, K) ->
    [#state{load=Load, requests=Requests}] = ets:lookup(ets_tester_db, Name),
    ets:update_element(ets_tester_db, Name,
        [{#state.load, Load-1}, {#state.requests, lists:delete(K, Requests)}]),
    dets:insert(dets_tester_db,
        #state{srv_name=Name, load=Load-1, requests=lists:delete(K, Requests)}).

get_requests(Name) ->
    [#state{requests=Requests}] = ets:lookup(ets_tester_db, Name),
    Requests.
