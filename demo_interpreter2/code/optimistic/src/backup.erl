-module(optimistic).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, 
	     handle_info/2, code_change/3]).


-export([start/1, stop/1, reset/2, delete/2, operation/3, commit/1, abort/1]).

-export([get_Pid2abort/3, check_intersection/2]).


%%%===================================================================
%%% API
%%%===================================================================


% The result of gen_server:start() is {ok,Pid} | ignore | {error,Error}
% S = Pid.
start(State) ->
	case is_map(State) of
		false -> {error, shared_state_is_not_map};
		true  ->
				Optimistic_Server = gen_server:start(?MODULE, State, []),
				case Optimistic_Server of
					{ok, S}   -> {ok, S};
					ignore    -> {error, ignore};
					ErrReason -> ErrReason
				end
	end.





stop(S) -> 
	try gen_server:call(S, stop) of
		{ok, State} -> {ok, State}
	catch
		_:Reason -> {error, Reason}
	end.


reset(S, State) ->
	try gen_server:call(S, {reset, State}) of
		ok -> ok
	catch
		_:Reason -> {error, Reason}
	end. 


delete(S, Keys) ->
	try gen_server:call(S, {delete, Keys}) of
		ok ->ok
	catch
		_:Reason -> {error, Reason}
	end.

% operation(_, _, _) -> not_implemented.

operation(S, Reads, OFun) ->
	try gen_server:call(S, {operation, Reads, OFun}) of
		{ok, OR} -> {ok, OR}
	catch
		_:Reason -> {error, Reason}
	end.	







commit(_) -> not_implemented.
abort(_) -> not_implemented.







%%%===================================================================
%%% gen_server callback funtions
%%%===================================================================

% maps to list: #{k1=>va, k2=>v2}  -> [{k1, v1}, {k2, v2}]
handle_call(stop, _From, State) -> 
	{Shared_State, OngoingMap} = State,
	OngoingList = maps:to_list(OngoingMap),
	lists:foreach(fun({_OR, {Pid, _RL, _WL, _DL}}) -> 
						gen_server:reply(Pid, aborted) end, OngoingList),
	NewState = {Shared_State, #{}},

	gen_server:stop(self()),
	{reply, {ok, Shared_State}, NewState};


handle_call({reset, ResetState}, _From, State) -> 
	{Shared_State, OngoingMap} = State,
	OngoingList = maps:to_list(OngoingMap),
	lists:foreach(fun({_OR, {Pid, _RL, _WL, _DL}}) -> 
					gen_server:reply(Pid, aborted) end, OngoingList),
	
	NewState = {ResetState, #{}},

	{reply, ok, NewState};


handle_call({delete, Keys}, _From, State) ->
    % io:format("~w ~n", [State]),
    % Keys = [key1, key2.....]
    % OngoingMap = 
    %    #{Key = Operation_Reference(OR), Value = {Operation_Pid, [read set], [write set], [dirty]}}.
    {Shared_State, OngoingMap} = State,
	OngoingList = maps:to_list(OngoingMap),
	AfterDel = lists:map(fun(Key) -> maps:remove(Key, Shared_State) end, Keys),

	PidList = get_Pid2abort(OngoingList, lists:usort(Keys), []),
	lists:foreach(fun(Pid) -> gen_server:reply(Pid, aborted) end, PidList),
    NewState = lists:last(AfterDel),
    % io:format("~w ~n", [NewState]),
	{reply, ok, NewState};


handle_call({operation, Reads, OFun}, _From, State) ->
	{Shared_State, OngoingMap} = State,
	ViewList = get_view(Shared_State, Reads, []),
	ViewMap = maps:from_list(ViewList),
	
	Me = self(),
	OR = make_ref(),
	spawn(fun() -> process_operation(Me, OR, OFun, ViewMap) end),


	NewState =
	{reply, ok, NewState}.


handle_cast(_, _) -> not_implemented.


% process_operation(From, OR, OFun, ViewMap) ->
% 	try OFun(ViewMap) of
% 		{Res, Change} -> From ! {OR, {Res, Change}}
% 	catch
% 		_: 


% traverse Reads and map find the sub map with the keys in Reads.
% get the list of tuple for sub map view.
get_view(Shared_State, Reads, ViewList) ->
	case Reads of
		[] -> ViewList;
		[Key | Rest] ->
			case maps:is_key(Key, Shared_State) of
				true  -> NewViewList = [{Key, maps:get(Key, Shared_State)} | ViewList],
						 get_view(Shared_State, Rest, NewViewList);
				false -> get_view(Shared_State, Rest, ViewList)
			end
	end.




get_Pid2abort(OngoingList, Keys, Container) ->
	case OngoingList of
		[] -> Container;
		[{_OR, {Pid, Reading, Writing, Dirty}} | Rest] ->
			case check_intersection(lists:usort(lists:append(Reading, Writing)), Keys) of
				false -> get_Pid2abort(Rest, Keys, [Pid | Container]);
				true -> get_Pid2abort(Rest, Keys, Container)
			end
	end.
	


check_intersection(Usort_dependency, Keys) ->
	Union = lists:append(Usort_dependency, Keys),
	New_length = length(lists:usort(Union)),
	Original_length = length(Usort_dependency) + length(Keys),
 	New_length =:= Original_length.






% OngoingMap = #{Key = Operation_Reference(OR), Value = {Operation_Pid, [read set], [write set], [dirty]}}.
init(Shared_State) -> 
	OngoingMap = #{},
	{ok, {Shared_State, OngoingMap}}.


terminate(_Reason, _State) ->
    ok.
	
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.











    
test_get_Pid2abort() ->
	{"Test get_Pid2abort",
		fun() ->
			OngoingList = [{1, {a, [r1, r2], [w1, w2], [d1]}}, 
						   {2, {b, [r3, r4], [], [d2]}}, 
						   {3, {c, [], [w3, w4], [d3]}}],
			
			Keys = [r1, k, y, w3],

			Expected = lists:sort([a, c]),
			?assertMatch(Expected, lists:sort(optimistic:get_Pid2abort(OngoingList, Keys, [])))
		end
	}.
