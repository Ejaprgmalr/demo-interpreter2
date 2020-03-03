-module(test_optimistic).
-include_lib("eqc/include/eqc.hrl").
-export([mkopr/2, good_opr/0, prop_opr_accurate/1, prop_server_commit/0, prop_isolated/0,
         test_all/0, test_everything/0]).

-compile(export_all).


mkopr(Oper, Args) ->
	case Oper of 
		incr -> fun(Args) -> Args + 1 end;
		% {incr, _} -> invalid;
		decr -> fun(Args) -> Args - 1 end;
		% {decr, _} -> invalid;
		swap -> fun(Args) ->
						{A, B} = Args ,
						{B, A}
					end;
		_ -> fun(Args) -> Args + 1 end
		% {swap, _} -> invalid
	end.






good_opr() -> 

	% RW_list = [key() || _ <- lists:seq(0, rand:uniform(10))],
	Opr = elements([incr, decr]),
	% case Opr of
	% 	incr -> Args = int();
	% 	decr -> Args = int();
	% 	swap -> Args = {key(), key()}
	% end,
	Args = int(),
	{call, test_optimistic, mkopr, [Opr, Args]}.






prop_oopr() -> eqc:equals(eval(good_opr()), 1).

% prop_opr_accurate(OprGen) ->
%     ?FORALL({K1, K2, SymOpr},
%             {[key()], [key()], OprGen},
%             eqc:equals(eval(OprGen),
%                        case K1 =:= K2 of
%                            true ->  {found, V};
%                            false -> find(K2, T)
%                        end)).




% good_opr()->
% 	?LAZY(
% 	oneof([{readset(),writeset(), {call, test_optimistic,mkopr,[incr,readset()]}},
% 	{readset(),writeset(), {call, test_optimistic,mkopr,[swap,readset()]}}
% 	])).	
	
% prop_opr_accurate()->
% 	?FORALL(I,good_opr(),Prop(I)).



prop_opr_accurate(_) -> not_implemented.
prop_server_commit() -> not_implemented.
prop_isolated() -> not_implemented.
test_all() -> not_implemented.
test_everything() -> not_implemented.






key() ->
	 oneof([int(), atom(), real(), char(), nat()]).

% key() ->
%     oneof([atom(), int(), real()]).

atom() ->
    elements([a,b,c,d]).

% atom() -> binary_to_atom(utf8()).