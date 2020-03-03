-module(operation).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, handle_event/4]).





%%%===================================================================
%%% gen_statem callback funtions
%%%===================================================================
callback_mode() ->
    handle_event_function.

init(SharedMap)->
	State = operation_inited,
	Data = {SharedMap, []},
	{ok, State, Data}.



terminate(_, _StateName, _StateData) ->
    void.


code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.


handle_event({call, From}, _, operation_inited, Data) ->
	NextState = complete,
	{next_}