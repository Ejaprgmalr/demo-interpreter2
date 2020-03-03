-module(eunit_helper).
% -export([conflicting_updates/1, nonconflicting_updates/1]).
-export([server/0]).






server()->
  {ok, S} = optimistic:start(#{1=>42, 2=>53, 3=>64}),
  {ok, OR} = optimistic:operation(S, [1], 
                                 fun(View) -> 
                                    V = maps:get(1, View), 
                                    {succ, #{1 => V+1}} 
                                  end),
  optimistic:abort(OR),
  optimistic:commit(OR).
  % optimistic:abort(OR).


% try_server(OR) ->
%  optimistic:commit(OR),
%  receive
%     {_,Result} -> Result
%  end.






% incr(S, C) ->
%     {ok, OR} = optimistic:operation(S, [C],
%                               fun (View) ->
%                                       V = maps:get(C, View),
%                                       {success, #{C => V+1}}
%                               end),
%     optimistic:commit(OR).

% updates(K1, K2, N) ->
%     {ok, S} = optimistic:start(#{K1 => 0, K2 => 0}),
%     Me = self(),
%     P1 = spawn(fun() -> Me ! {self(),
%                               lists:map(fun(_) -> incr(S, K1) end,
%                                         lists:seq(1, N))}
%                end),
%     P2 = spawn(fun() -> Me ! {self(),
%                               lists:map(fun(_) -> incr(S, K2) end,
%                                         lists:seq(1, N))}
%                end),
%     L2 = receive {P2, Res2} -> Res2 end,
%     L1 = receive {P1, Res1} -> Res1 end,
%     {ok, #{K1 := C1, K2 := C2}} = optimistic:stop(S),
%     {C1, length([ R || {ok, R} <- L1]),
%      C2, length([ R || {ok, R} <- L2])}.

% conflicting_updates(N) ->
%     updates(crash, crash, N).

% nonconflicting_updates(N) ->
%     updates(x, y, N).
