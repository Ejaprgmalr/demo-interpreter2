-module(test_eunit_optimistic).
-export([test_all/0]).

%% Maybe you want to use eunit

-include_lib("eunit/include/eunit.hrl").


test_all() ->
    eunit:test(
      [
       test_start_optimistic(),
       test_start_optimistic_withoutMap(),
       test_stop_optimistic(),
       test_reset_shared_state(),
       test_reset_content(),
       test_delete_keys(),
       test_after_delete_shared_state(),
       test_delete_non_existing_keys(),
       test_delete_nothing(),
       test_operation_reply(),
       test_commit_reply(),
       test_commit_twice(),
       test_abort_too_late(),
       test_abort(),
       test_commit_after_abort(),
       test_

      ], [verbose]).





test_start_optimistic() -> 
	{"Positive Test: start an optimistic server",
		fun() ->
			?assertMatch({ok, _}, optimistic:start(#{}))
		end
	}.


test_start_optimistic_withoutMap() ->
	{"Negative Test: start an optimistic server with shared state is not map",
		fun() ->
			Expected = {error, shared_state_is_not_map},
			?assertMatch(Expected, optimistic:start([]))
		end
	}.



test_stop_optimistic() ->
	{"Positive Test: stop an optimistic server",
		fun() ->
			SharedState = #{},
			Expected = {ok, SharedState},
			{ok, S} = optimistic:start(SharedState),
			?assertMatch(Expected, optimistic:stop(S))
		end	
	}.


test_reset_shared_state() ->
	{"Positive Test: reset shared state of an optimistic server",
		fun() ->
			NewSharedState = #{a=>1, b=>2},
			{ok, S} = optimistic:start(#{}),
			?assertMatch(ok, optimistic:reset(S, NewSharedState))
		end
	}.

test_reset_content()->
	{"Test the shared state is reseted as Expected",
		fun() ->
			NewSharedState = #{a=>1, b=>2},
			{ok, S} = optimistic:start(#{}),
			optimistic:reset(S, NewSharedState),
			Expected = {ok, NewSharedState},
			?assertMatch(Expected, optimistic:stop(S))
		end
	}.

test_delete_keys() ->
	{"Positive Test: delete keys from shared state and return ok",
		fun() ->
			{ok, S} = optimistic:start(#{a=>1, b=>2, c=>3}),
			Keys = [a, b],
			?assertMatch(ok, optimistic:delete(S, Keys))
		end
	}.

test_after_delete_shared_state() ->
	{"Check the shared state after delete",

		fun() ->
			Initial = #{a=>1, b=>2, c=>3, d=>4},
			{ok, S} = optimistic:start(Initial),
			Keys = [b, d],
			optimistic:delete(S, Keys),
			Newshared_state = #{a => 1,c=>3},
			?assertMatch({ok, Newshared_state}, optimistic:stop(S))
		end
	}.

test_delete_non_existing_keys() ->
	{"Test delete non existing keys",
		fun() ->
			Initial = #{a=>1, b=>2, c=>3, d=>4},
			{ok, S} = optimistic:start(Initial),
			Keys = [b, d, non, ok],
			optimistic:delete(S, Keys),
			Newshared_state = #{a => 1,c=>3},
			?assertMatch({ok, Newshared_state}, optimistic:stop(S))
		end
    }.

test_delete_nothing() ->
	{"Test delete nothing, the shared state should not change",
		fun() ->
			Initial = #{a=>1, b=>2, c=>3, d=>4},
			{ok, S} = optimistic:start(Initial),
			optimistic:delete(S, []),
			?assertMatch({ok, Initial}, optimistic:stop(S))
		end	
	}.

test_operation_reply() ->
	{"Test the operation reply {ok, OR}, 
	  my OR ={loopPid, Ref}",
	    fun() ->
	  		Initial = #{a=>1, b=>2, c=>3, d=>4},
			{ok, S} = optimistic:start(Initial),
			OFun = fun(View) -> View, {succ, #{}} end,
			?assertMatch({ok, {_Pid, _Ref}}, optimistic:operation(S, [a], OFun))


		end
	}.




test_commit_reply() ->
	{"Test commit get reply {ok, Res}",
			fun()->
			{ok, S} = optimistic:start(#{1=>42, 2=>53, 3=>64}),
			{ok, OR} = optimistic:operation(S, [1], 
		   						fun(View) -> 
		   							V = maps:get(1, View), 
		   							{succ, #{1 => V+1}} 
		   						end),
			?assertMatch({ok, succ}, optimistic:commit(OR))
		end
	}.



test_commit_twice() ->
	{"Test the result of call commit twice with the same arg are the same",
		fun()->
			{ok, S} = optimistic:start(#{1=>42, 2=>53, 3=>64}),
			{ok, OR} = optimistic:operation(S, [1], 
		   						fun(View) -> 
		   							V = maps:get(1, View), 
		   							{succ, #{1 => V+1}} 
		   						end),
			Res1 = optimistic:commit(OR),
			Res2 = optimistic:commit(OR),
			?assertMatch(Res1, Res2)
		
		end
	}.


test_abort_too_late() ->
	{"Test abort after commit",
		fun() ->
			{ok, S} = optimistic:start(#{1=>42, 2=>53, 3=>64}),
			{ok, OR} = optimistic:operation(S, [2], 
											fun(View) -> 
												View,
												{same, View}
											end),
			optimistic:commit(OR),
			?assertMatch(too_late, optimistic:abort(OR))
		end
			
	}.


test_abort() ->
	{"Test abort before commit",
		fun() ->
			{ok, S} =optimistic:start(#{1=>42, 2=>53, 3=>64}),
			{ok, OR} = optimistic:operation(S, [3], 
											 fun(View) ->
											 	View,
											 	{yes, View}
											 end),
			?assertMatch(aborted, optimistic:abort(OR))
		end

	}.

test_commit_after_abort() ->
	{"Test cmmit after abort",
		fun() ->
			{ok, S} =optimistic:start(#{1=>42, 2=>53, 3=>64}),
			{ok, OR} = optimistic:operation(S, [3], 
											 fun(View) ->
											 	View,
											 	{yes, View}
											 end),
			optimistic:abort(OR),
			?assertMatch(aborted, optimistic:commit(OR))
		end		

	}.









% commit_reply() ->
% 	{"commit positive test",
% 		fun() ->
% 		   {ok, S} = optimistic:start(#{1=>42, 2=>53, 3=>64}),
% 		   {ok, OR}=optimistic:operation(S, [1], 
% 		   						fun(View) -> 
% 		   							V = maps:get(1, View), 
% 		   							{succ, #{1 => V+1}} 
% 		   						end),
% 		   Me = self(),
% 		   Pid = spawn(fun()-> optimistic:commit(OR),
% 		   				 receive
% 			   				 {_, Result} -> Me ! {self(), Result}
% 		   				  end

% 		   		end),
% 		   receive 
% 		   		{Pid, Result} -> ?assertMatch({ok, succ},Result)
% 		   end
%  		end
% 	}.



% abort_positive() ->
% 	{"positive test for abort function",

% 		fun() ->
% 		   {ok, S} = optimistic:start(#{1=>42, 2=>53, 3=>64}),
% 		   {ok, OR}=optimistic:operation(S, [1], 
% 		   						fun(View) -> 
% 		   							V = maps:get(1, View), 
% 		   							{succ, #{1 => V+1}} 
% 		   						end),

% 		   Result = optimistic:abort(OR),
% 		   ?assertMatch({aborted}, Result)
%  		end

% 	}.






% stop_optimistic() ->
% 	{"Test stop an optimistic server",
% 		fun() ->
% 			{ok, S} = optimistic:start(#{}),

% 	}



% test_get_Pid2abort() ->
% 	{"Test get_Pid2abort",
% 		fun() ->
% 			OngoingList = [{a, {[r1, r2], [w1, w2]}}, {b, {[r3, r4], []}}, {c, {[], [w3, w4]}}],
			
% 			Keys = [r1, k, y, w3],

% 			Expected = lists:sort([a, c]),
% 			?assertMatch(Expected, lists:sort(optimistic:get_Pid2abort(OngoingList, Keys, [])))
% 		end
% 	}.



% statistics() -> 
% 	{"Test statistics",
% 	fun() ->
% 		{ok, Ref} = rps:start(),
% 		?assertMatch({ok,0,0,0}, rps:statistics(Ref)),
% 		spawn(fun()-> rps:queue_up(Ref,test, 3) end),
% 		rps:queue_up(Ref,test1, 3),
% 		?assertMatch({ok,0,0,1}, rps:statistics(Ref))
% 	end}.
% start_broker() ->
%     {"Start a broker, and nothing else",
%      fun() ->
%              ?assertMatch({ok, _}, rps:start())
%      end}.
% queue_up() ->
% 	{"Test queue_up",
% 	fun() ->
% 		{ok, Ref} = rps:start(),
% 		spawn(fun()-> ?assertMatch({ok,_,_}, rps:queue_up(Ref,test, 3)) end),
% 		?assertMatch({ok,_,_}, rps:queue_up(Ref,test1, 3))
% 	end}.
% move() ->
% 	{"Test move",
% 	fun() ->
% 		{ok, Ref} = rps:start(),
% 		spawn(fun()-> 
% 			{ok,_,C}=rps:queue_up(Ref,test, 3),
% 			?assertMatch(tie,rps:move(C,rock))
% 		end),
% 		{ok,_,C}=rps:queue_up(Ref,test1, 3),
% 		?assertMatch(tie,rps:move(C,rock))
% 	end}.
% rock_bots () ->
% 	{"Two rock_bots, should tie forever",
% 	 fun() ->
% 		{ok, Ref} = rps:start(),
% 		spawn(fun()-> rock_bot:queue_up_and_play(Ref) end),
% 		{ok,A,B} = rock_bot:queue_up_and_play(Ref),
% 		?assertEqual(A,B)
% 	 end}.
% paper_rock_bots () ->
% 	{"paper bot vs rock bot",
% 	 fun() ->
% 		{ok, Ref} = rps:start(),
% 		spawn(fun()-> rock_bot:queue_up_and_play(Ref) end),
% 		{ok,A,B} = paper_bot:queue_up_and_play(Ref),
% 		?assertMatch(A,2),
% 		?assertMatch(B,0)
% 	 end}.
