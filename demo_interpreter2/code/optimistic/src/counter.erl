-module(counter).
-export([conflicting_updates/1, nonconflicting_updates/1]).


incr(S, C) ->
    {ok, OR} = optimistic:operation(S, [C],
                              fun (View) ->
                                      V = maps:get(C, View),
                                      {success, #{C => V+1}}
                              end),
    optimistic:commit(OR).

updates(K1, K2, N) ->
    {ok, S} = optimistic:start(#{K1 => 0, K2 => 0}),
    % io:format("~w ~n", [])
    Me = self(),
    P1 = spawn(fun() -> Me ! {self(),
                              lists:map(fun(_) -> incr(S, K1) end,
                                        lists:seq(1, N))}
               end),
    P2 = spawn(fun() -> Me ! {self(),
                              lists:map(fun(_) -> incr(S, K2) end,
                                        lists:seq(1, N))}
               end),
    L2 = receive {P2, Res2} -> Res2 end,
    L1 = receive {P1, Res1} -> Res1 end,
    % io:format("L1~w ~n", [L1]),
    % io:format("L2~w ~n", [L2]),
    
    {ok, #{K1 := C1, K2 := C2}} = optimistic:stop(S),
    {C1, length([ R || {ok, R} <- L1]), 
     C2, length([ R || {ok, R} <- L2])}.

conflicting_updates(N) ->
    updates(crash, crash, N).

nonconflicting_updates(N) ->
    updates(x, y, N).
