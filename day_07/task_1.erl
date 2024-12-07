-module(task_1).

-export([main/1]).
main(File) ->
	{ok, Input} = file:read_file(File),
	Lines = binary:split(Input, <<"\n">>, [global, trim_all]),
	Calibrations = lists:map(fun(Line) -> parse(binary_to_list(Line)) end, Lines),
	lists:foldl(fun({Result, Operands}, Sum) -> Sum + solve(Result, Operands) end, 0, Calibrations).

parse(Line) ->
	parse(Line, [0]).
parse([], Nums) -> 
	[Result|Operands] = lists:reverse(Nums),
	{Result, Operands};
parse([H|T], [NumsH|NumsT] = Nums) -> 
	parse(
	  	T,
		case H of
			$\s -> [0 | Nums];
			Char when Char >= $0, Char =< $9 -> [NumsH*10+(Char-$0) | NumsT];
			_ -> Nums
		end
	).

solve(Res, [H|T]) ->
	case lists:any(fun(Calc) -> Calc == Res end, calc_branches(T, [H])) of
		true -> Res;
		false -> 0
	end.

calc_branches([], Calcs) ->
	Calcs;
calc_branches([H|T], Calcs) ->
	calc_branches(
		T,
		% task 1 and 2 differ only by presence of the third element produced by `concat`
		lists:foldl(fun(Calc, Acc) -> [Calc + H, Calc * H, concat(Calc, H) | Acc] end, [], Calcs)
	).

concat(X, Y) ->
	X * math:pow(10, trunc(math:log10(Y)) + 1) + Y.
