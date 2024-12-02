-module(task_1).

-export([main/0]).

main() ->
	{ok, Input} = file:read_file("input_1.txt"),
	Lines = binary:split(Input, <<"\n">>, [global, trim_all]),
	IntLines = [begin
		Vals = binary:split(Line, <<"\s">>, [global, trim_all]),
		lists:map(fun(Val) -> binary_to_integer(Val) end, Vals)
	 end || Line <- Lines],
	solve(IntLines).

solve(Lines) ->
	solve(Lines, 0).
solve([], Sum) ->
	Sum;
solve([H|T], Sum) -> 
	solve(T, Sum + estimate_safety(H)).

estimate_safety(Nums) ->
	estimate_safety(lists:nthtail(1, Nums), lists:nth(1, Nums), 0, true).
estimate_safety([H], Prev, Dir, _) when Dir == 0; abs(Dir) > 4; Dir > 0, H - Prev < 0; Dir < 0, H - Prev > 0 ->
	0;
estimate_safety([H|T], Prev, _, _) ->
	estimate_safety(T, H, H - Prev, false);
estimate_safety(_, _, _, _)  ->
	1.
