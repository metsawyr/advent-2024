-module(day_08).

-export([main/1]).

main(File) ->
	{ok, Input} = file:read_file(File),
	{Data, Cols, Len} = process_input(Input),
	Rows = Len div Cols,
	Antennas = maps:groups_from_list(fun({Frequency, _}) -> Frequency end, fun({_, Index}) -> Index end, Data),
	Pairs = maps:fold(fun(_, Value, Acc) -> Acc ++ combinations(Value) end, [], Antennas),
	PairedAntennas = maps:fold(fun(_, Value, Acc) when length(Value) > 1 -> Acc ++ Value end, [], Antennas),
	
	Solution1 = length(lists:uniq(get_antinodes(Pairs, {Cols, Rows}, 1))),
	Solution2 = length(lists:uniq(get_antinodes(Pairs, {Cols, Rows}, -1) ++ PairedAntennas)), 
	{
		Solution1,
		Solution2
	}.

process_input(Input) ->
	process_input(Input, 0, [], -1).
process_input(<<>>, Index, Output, LineLen) -> 
	{Output, LineLen, Index};
process_input(<<H,T/binary>>, Index, Output, LineLen) ->
	case H of 
		$\n ->
			Len = case LineLen of -1 -> Index; _ -> LineLen end,
			process_input(T, Index, Output, Len);
		$. ->
			process_input(T, Index+1, Output, LineLen);
		_ ->
			process_input(T, Index+1, [{H, Index}|Output], LineLen)
	end.

combinations([]) -> [];
combinations([H|T]) -> [{X, H} || X <- T] ++ combinations(T).

get_antinodes(Pairs, Grid, Steps) ->
	get_antinodes(Pairs, Grid, Steps, []).
get_antinodes([], _, _, Antinodes) ->
	Antinodes;
get_antinodes([{X, Y} | T], {_, Cols} = Grid, Steps, Antinodes) ->
	XCol = X rem Cols,
	XRow = X div Cols,
	YCol = Y rem Cols,
	YRow = Y div Cols,

	ColDelta = YCol - XCol,
	RowDelta = YRow - XRow,

	get_antinodes(
		T, 
		Grid,
		Steps,
		Antinodes ++ extrapolate_antinodes(Grid, {XRow, XCol}, {RowDelta, ColDelta}, Steps) ++ extrapolate_antinodes(Grid, {YRow, YCol}, {-RowDelta, -ColDelta}, Steps)
	).

extrapolate_antinodes(Grid, Pos, Delta, Iters) ->
	extrapolate_antinodes(Grid, Pos, Delta, Iters, []).
extrapolate_antinodes(_, _, _, 0, Antinodes) -> 
	Antinodes;
extrapolate_antinodes({_, Cols} = Grid, {Row, Col}, {RowDelta, ColDelta} = Delta, Iters, Antinodes) -> 
	Pos = {Row - RowDelta, Col - ColDelta},
	case in_bounds(Grid, Pos) of
		true -> extrapolate_antinodes(Grid, Pos, Delta, Iters - 1, [to_index(Cols, Pos) | Antinodes]);
		false -> Antinodes
	end.

in_bounds({Rows, Cols}, {Row, Col}) ->
	(Row >= 0) andalso (Row < Rows) andalso (Col >= 0) andalso (Col < Cols).

to_index(Cols, {Row, Col}) -> Row * Cols + Col.
