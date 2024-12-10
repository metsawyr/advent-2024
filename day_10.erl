-module(day_10).

-export([main/1]).

main(File) ->
	{ok, Input} = file:read_file(File),
	{Map, Len, Cols, Nines} = process_input(Input),
	Rows = Len div Cols,
	erlang:display({"Cols", Cols, "Len", Len, "Rows", Rows, "Nines", Nines}),
	traverse_map(Map, Rows, Cols, Nines).

process_input(Input) ->
	process_input(Input, 0, -1, <<>>, []).
process_input(<<>>, Index, Cols, Map, Nines) ->
	{Map, Index, Cols, lists:reverse(Nines)};
process_input(<<H, T/binary>>, Index, Cols, Map, Nines) ->
	case H of 
		$\n -> 
			process_input(T, Index, case Cols of -1 -> Index; _ -> Cols end, Map, Nines);
		$9 ->
			process_input(T, Index+1, Cols, <<Map/binary, H>>,[Index | Nines]);
		_ ->
			process_input(T, Index+1, Cols, <<Map/binary, H>>, Nines)
	end.

probe_dir(Rows, Cols, Pos, Dir) ->
	Row = Pos div Cols,
	Col = Pos rem Cols,
	case Dir of
		e when Col > 0 -> {ok, Pos - 1};
		w when Col < (Cols - 1) -> {ok, Pos + 1};
		n when Row > 0 -> {ok, Pos - Cols};
		s when Row < (Rows - 1) -> {ok, Pos + Cols};
		_ -> {oob}
	end.

traverse_map(Map, Rows, Cols, Trails) ->
	traverse_map(Map, Rows, Cols, Trails, 0).
traverse_map(_, _, _, [], Score) ->
	Score;
traverse_map(Map, Rows, Cols, [Trail | RestTrails], Score) ->
	TrailScore = score_trailhead(Map, Rows, Cols, Trail),
	erlang:display({"Trail", Trail, "Score", TrailScore}),
	traverse_map(Map, Rows, Cols, RestTrails, Score + TrailScore).

score_trailhead(Map, Rows, Cols, Pos) ->
	lists:foldl(fun(Dir, Acc) -> Acc + score_trailhead(Map, Rows, Cols, Pos, Dir, $9) end, 0, [e, w, n, s]).
score_trailhead(Map, Rows, Cols, Pos, Dir, Prev) ->
	case probe_dir(Rows, Cols, Pos, Dir) of
		{ok, NextPos} ->
			Value = binary:at(Map, NextPos),
	erlang:display({"Pos", Pos, "NextPos", NextPos, "Value", Prev, "NextValue", Value}),
			case Prev - Value of
				1 when Value == $0 -> erlang:display("exit 1"), 1;
				1 -> lists:foldl(fun(NextDir, Acc) -> Acc + score_trailhead(Map, Rows, Cols, NextPos, NextDir, Value) end, 0, [e, w, n, s]);
				_ -> erlang:display("exit 0"), 0
			end;
		{oob} -> 0
	end.


