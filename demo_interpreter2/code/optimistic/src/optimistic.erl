-module(optimistic).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, 
	     handle_info/2, code_change/3]).


-export([start/1, stop/1, reset/2, delete/2, operation/3, commit/1, abort/1]).

% -export([get_Pid2abort/3, check_intersection/2, delete_sharedstate_keys/2]).


%%%===================================================================
%%% API
%%%===================================================================

% The result of gen_server:start() is {ok,Pid} | ignore | {error,Error}
% S = Pid.
start(State) ->
	case is_map(State) of
		false -> 
			{error, shared_state_is_not_map};
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


operation(S, Reads, OFun) ->
	try gen_server:call(S, {operation, Reads, OFun}) of
		{ok, OR} -> {ok, OR}
	catch
		_:Reason -> {error, Reason}
	end.	


commit(OR) -> 
	{LoopPid, Ref} = OR,
	Me = self(),
	LoopPid ! {Ref, Me, commit},

	receive
		{Ref, {ok, Res}} -> {ok, Res};
		{Ref, aborted} -> aborted
	end.


abort(OR) ->
	{LoopPid, Ref} = OR,	 
	Me = self(),
	LoopPid ! {Ref, Me, abort},
	receive
		{Ref, aborted} -> aborted;
		{Ref, too_late} -> too_late;
		{Ref, {error, Reason}} -> {error, Reason}
	end.

%%%===================================================================
%%% gen_server callback funtions
%%%===================================================================

% Operation_Pool = #{Key = OR, Value = {[read set], [dirty set], state_flag}}.
% OR = {loopPid, Ref}
init(Shared_State) -> 
	Operation_Pool = #{},
	{ok, {Shared_State, Operation_Pool}}.

terminate(_Reason, _State) ->
    ok.
	
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% _From = {Pid, Tag}????
% OR = {LoopPid, Ref}
% OperationPool = {Key = OR, Value = {Read list, Ditrty list, statefla}}


% maps to list: #{k1=>va, k2=>v2}  -> [{k1, v1}, {k2, v2}]
handle_call(stop, _From, State) -> 
	{Shared_State, Operation_Pool} = State,
	Operation_PoolKeys = maps:keys(Operation_Pool),
	abortAllOngoing(Operation_PoolKeys),

	NewState = {Shared_State, #{}},
	{stop, normal, {ok, Shared_State}, NewState};



handle_call({reset, ResetState}, _From, State) -> 
	{_Shared_State, Operation_Pool} = State,
	Operation_PoolKeys = maps:keys(Operation_Pool),
	abortAllOngoing(Operation_PoolKeys),

	NewState = {ResetState, #{}},
	{reply, ok, NewState};


handle_call({delete, Keys}, _From, State) ->  
    % Keys = [key1, key2.....]
    {Shared_State, Operation_Pool} = State,
    % io:format("SharedState~w ~n", [Shared_State]),
	AllKeys_Shared_state = maps:keys(Shared_State),
    Delete_Keys = lists:takewhile(fun(K) -> 
    								lists:member(K, AllKeys_Shared_state) 
    							  end, Keys),

    NewShared_State = delete_sharedstate_keys(Keys, Shared_State),
    % io:format("NewShared_State~w ~n", [NewShared_State]),    
	OperationLis = maps:to_list(Operation_Pool),
	New_Operation_Pool = upAllDLis(OperationLis, Delete_Keys, []),
  

    NewState = {NewShared_State, New_Operation_Pool},
	{reply, ok, NewState};



handle_call({operation, Reads, OFun}, _From, State) ->
	{Shared_State, Operation_Pool} = State,
	ViewList = get_view(Shared_State, Reads, []),
	ViewMap = maps:from_list(ViewList),
	% io:format("View~w ~n", [ViewMap]),
	
	Me = self(),
	Ref = make_ref(),
    % [] is the state of the loop used to store result and state flage
	LoopPid = spawn(fun() -> loop(Ref, Me, []) end),
	LoopPid ! {Ref, Me, {operation, OFun, ViewMap}},
	OR = {LoopPid, Ref},
	% dirty set = []
	% State_flag = operating | commited | aborted
	NewOperation_Pool = maps:put(OR, {Reads, [], operating}, Operation_Pool),
	NewState = {Shared_State, NewOperation_Pool},
	{reply, {ok, OR}, NewState};



handle_call({commit_from_loop, Ref, LoopPid, {Res, Change}}, _From, State) ->
	Me = self(),
	WLis = maps:keys(Change),



	{Shared_State, Operation_Pool} = State,
	ComeOR = {LoopPid, Ref},

	{RLis, DLis, Stateflag} = maps:get(ComeOR, Operation_Pool),
	DepenLis = lists:append(RLis, WLis),
	InterSection = lists:takewhile(fun(E) -> lists:member(E, DepenLis) end, DLis),
	% io:format("~w ~n", [before_check_stateflag]),
    % State_flag = operating | commited | aborted
	case {Stateflag, length(InterSection)} of

		{aborted, _} -> 
					% io:format("~w ~n", [abortit]),
					LoopPid ! {Ref, Me, failed_commit},
				  % UpNewOperation_Pool = maps:update(ComeOR, {RLis, DLis, aborted}, Operation_Pool),
				    {reply, {Ref, Me, failed_commit}, State};
		{_, 0} -> 
				  % io:format("~w ~n", [commitit]),
				  NewOperation_Pool = 
				  	upOtherDLis(maps:to_list(Operation_Pool), ComeOR, WLis, []),
				  
				  UpNewOperation_Pool = 
				  	maps:update(ComeOR, {RLis, DLis, commited}, NewOperation_Pool),
				  
				  NewShared_State = 
				  	updateShared_State(maps:to_list(Change), Shared_State),
				  
				  NewState = {NewShared_State, NewOperation_Pool},
				  % io:format("~w ~n", [here_optimistic]),
				  % gen_server:reply(LoopPid, {Ref, Me, success_commmit}),
				  LoopPid ! {Ref, Me, success_commmit}, 
				  {reply, {Ref, Me, success_commmit}, NewState};


		{_, _} -> 
			      % UpNewOperation_Pool = maps:update(ComeOR, {RLis, DLis, aborted}, Operation_Pool),
				  LoopPid ! {Ref, Me, failed_commit},
				  {reply, {Ref, Me, failed_commit}, State}
	end;


handle_call({commit_abort_from_loop, Ref, LoopPid, aborted}, _From, State) ->
	Me = self(),
	% io:format("~w ~n", [inhandle_abort]),
	{Shared_State, Operation_Pool} = State,
	ComeOR = {LoopPid, Ref},
	{RLis, DLis, Stateflag} = maps:get(ComeOR, Operation_Pool),
	case Stateflag of
		aborted -> LoopPid ! {Ref, Me, abort_updated}, 
				   {reply, {Ref, Me, abort_updated}, State};

		_ -> New_Operation_Pool = maps:put(ComeOR, {RLis, DLis, aborted}, Operation_Pool),
			 NewState = {Shared_State, New_Operation_Pool},
			 LoopPid ! {Ref, Me, abort_updated}, 
			 {reply, {Ref, Me, abort_updated}, NewState}
	end.

	% New_Operation_Pool = maps:put(ComeOR, {RLis, DLis, aborted}, Operation_Pool),
	% NewState = {Shared_State, New_Operation_Pool},
	% LoopPid ! {Ref, Me, abort_updated}, 
	% {reply, {Ref, Me, abort_updated}, NewState}.

	

handle_cast(_, _) -> not_implemented.



%%%===================================================================
%%% operation loop  and process for process operation
%%%===================================================================

loop(Ref, Creator, State) ->
    % State is the state of the loop used to store result and state flage
	Me = self(),
	receive
		{Ref, Creator, {operation, OFun, ViewMap}} -> 
			% io:format("InLoop~w ~n", [inloop]),
			Fun_Pid=spawn(fun() -> process_operation(Ref, Me, OFun, ViewMap) end),
			loop(Ref, Creator, State);

		{Ref, Fun_Pid, {Res, Change}} ->
			NewState = [not_commit, {Res, Change}],
			loop(Ref, Creator, NewState);

		{Ref, Fun_Pid, aborted} -> 
			NewState = [aborted],
			loop(Ref, Creator, NewState);
		{Ref, From, commit} ->
			% io:format("State~w ~n", [State]),
			case State of
				[] ->
					receive
						{Ref, Fun_Pid, {Res, Change}} ->
							gen_server:call(Creator, {commit_from_loop, Ref, Me, {Res, Change}}),
							receive 
								{Ref, Creator, success_commmit} -> 
									NewState = [commited, {Res, Change}],
									From ! {Ref, {ok, Res}}, 
									loop(Ref, Creator, NewState);

								{Ref, Creator, failed_commit} -> 

									NewState = [aborted],
									From ! {Ref, aborted}, 
									loop(Ref, Creator, NewState)
								end;

						{Ref, Fun_Pid, aborted} ->
							gen_server:call(Creator, {commit_abort_from_loop, Ref, Me, aborted}),
							receive 
								{Ref, Creator, aborted} -> ok
							end,
							NewState = [aborted],
							loop(Ref, Creator, NewState)
					end;

				[aborted] -> gen_server:call(Creator, {commit_abort_from_loop, Ref, Me, aborted}),
							 NewState = [aborted],
							 loop(Ref, Creator, NewState);

				[not_commit, {Res, Change}] -> 
								% io:format("NotEmpyty~w ~n", [{Res, Change}]),
						gen_server:call(Creator, {commit_from_loop, Ref, Me, {Res, Change}}),
							receive
								{Ref, Creator, success_commmit} ->
								   	From ! {Ref, {ok, Res}},
								   	NewState = [commited, {Res, Change}],
								   	loop(Ref, Creator, NewState);

								{Ref, Creator, failed_commit} -> 
								   		% io:format("~w ~n", [failed_commit]),
								   	From ! {Ref, aborted},
								   	NewState = [aborted],
								   	loop(Ref, Creator, NewState)
							end;

				[commited, {Res, Change}] ->
							From ! {Ref, {ok, Res}},
							loop(Ref, Creator, State)


			end;



		{Ref, From, abort} ->
			% io:format("HereState~w ~n", [State]),
			case State of
				[] -> 
					  % NewState = [aborted],
					  % io:format("Here~w ~n", [NewState]),
					  gen_server:call(Creator, {commit_abort_from_loop, Ref, Me, aborted}),
					  receive 
						{Ref, Creator, abort_updated} -> 
							% io:format("~w ~n", [receivkongkongkong]),
							ok
					  end,
					  NewState = [aborted],
				      From ! {Ref, aborted},
				      % io:format("~w ~n", [beforeloop]),
					  loop(Ref, Creator, NewState);

				[aborted] -> 
					  		 From ! {Ref, aborted},
					  		 loop(Ref, Creator, State);
				[commited, {Res, Change}]-> 
						From ! {Ref, too_late},
						loop(Ref, Creator, State);
				[not_commit, {Res, Change}] ->
						gen_server:call(Creator, {commit_abort_from_loop, Ref, Me, aborted}),
					    receive 
							{Ref, Creator, abort_updated} -> 
							% io:format("~w ~n", [receivenotcommit]),
							ok
					  	end,
						From ! {Ref, aborted},
						NewState = [aborted],
						loop(Ref, Creator, NewState)
			end

	end.
	




process_operation(Ref, From, OFun, ViewMap) ->
	Me = self(),

	try OFun(ViewMap) of
		{Res, Change} -> 
			% io:format("~w ~n", [process_operation]),
			From ! {Ref, Me, {Res, Change}}
	catch
		_:_ -> 	From ! {Ref, Me, aborted}
	end.



%%%===================================================================
%%% gen_server helper funtions
%%%===================================================================

% abort all the ongoing operation
% used by API : stop and reset
abortAllOngoing(OprPoolKeys) ->
	case OprPoolKeys of
		[] -> ok;
		[{Pid, _}| Rest] ->
				Pid ! aborted,
				abortAllOngoing(Rest)
	end.


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
		[{_OR, {Pid,Reading, Writing}} | Rest] ->
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





delete_sharedstate_keys(Keys, Shared_State) ->
	case Keys of
		[] -> Shared_State;
		[K1 | Rest] ->
			  NewShared_State = maps:remove(K1, Shared_State),
			  delete_sharedstate_keys(Rest, NewShared_State)
	end.



upAllDLis(Operation_Pool_list, Keys, Container) ->
	case Operation_Pool_list of
		[] -> 	
				% io:format("~w ~n", [Container]),
				maps:from_list(Container);
		[{Current_OR, {Read_list, Dirty_list, State_flag}} | Rest] ->
			New_Dirty_list = lists:append(Dirty_list, Keys),
			New_Container = [{Current_OR, {Read_list, New_Dirty_list, State_flag}} | Container],
			upAllDLis(Rest, Keys, New_Container)
	end.


upOtherDLis(Operation_Pool_list, ComeOR, WLis, Container) ->
	case Operation_Pool_list of
		[] -> maps:from_list(Container);
		[{Current_OR, {Read_list, Dirty_list, State_flag}} | Rest] ->
			case Current_OR of
				ComeOR -> 
					New_Container = [{Current_OR, {Read_list, Dirty_list, State_flag}} | Container],
					upOtherDLis(Rest, ComeOR, WLis, New_Container);
				OtherOR -> 
					New_Dirty_list = lists:append(Dirty_list, WLis),
					New_Container = [{Current_OR, {Read_list, New_Dirty_list, State_flag}} | Container],
					upOtherDLis(Rest, ComeOR, WLis, New_Container)
				end
	end.


updateShared_State(Change_list, Container) ->
	case Change_list of
		[] -> Container;
		[{Key, Value} | Rest] -> 
			New_Container = maps:put(Key, Value, Container),
			updateShared_State(Rest, New_Container)
	end.