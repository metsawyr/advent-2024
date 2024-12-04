-module(task_1).

-export([main/1]).

main(File) ->
	{ok, Input} = file:read_file(File),
	LineLen = line_len(Input),
	Data = binary:replace(Input, <<$\n>>, <<>>, [global]),

	count_xmas(Data, LineLen).

line_len(Data) ->
	line_len(Data, 0).
line_len(Data, Idx) when Idx >= byte_size(Data) ->
	0;
line_len(Data, Idx) ->
	case binary:at(Data, Idx) of
		$\n -> Idx;
		_ -> line_len(Data, Idx + 1)
	end.

count_xmas(Data, LineLen) ->
	count_xmas(Data, LineLen, 0, 0).
count_xmas(Data, _, Idx, Sum) when Idx >= byte_size(Data) ->
	Sum;
count_xmas(Data, LineLen, Idx, Sum) ->
	Dirs = predict_directions(Data, LineLen, Idx),
	MatchedWords = probe_dirs(Data, LineLen, Idx, Dirs),
	count_xmas(
	  Data, 
	  LineLen, 
	  Idx + 1, 
	  Sum + MatchedWords
	).

predict_directions(Data, LineLen, Idx) ->
	Col = Idx rem LineLen,
	Row = Idx div LineLen,	
	RowLen = byte_size(Data) div LineLen,

	WestGap = Col > 2,
	EastGap = (LineLen - Col) > 3,
	NorthGap = Row > 2,
	SouthGap = (RowLen - Row) > 3,

	 [Dir || {Condition, Dir} <- 
        [
		 {WestGap, w},
		 {EastGap, e},
		 {NorthGap, n},
		 {SouthGap, s},
         {SouthGap andalso WestGap, sw}, 
		 {SouthGap andalso EastGap, se}, 
         {NorthGap andalso WestGap, nw},
		 {NorthGap andalso EastGap, ne}
		], 
        Condition].

probe_dirs(Data, LineLen, Idx, Dirs) ->
	probe_dirs(Data, LineLen, Idx, Dirs, 0).
probe_dirs(_, _, _, [], Sum) ->
	Sum;
probe_dirs(Data, LineLen, Idx, [Dir|Dirs], Sum) ->
	probe_dirs(
		Data,
		LineLen,
		Idx,
		Dirs,
		Sum + traverse_dir(Data, LineLen, Idx, Dir, 0)
	).

traverse_dir(_, _, _, _, 4) -> 1;
traverse_dir(Data, LineLen, Idx, Dir, Iter) ->
	Char = binary:at(Data, Idx),
	case Iter of
		0 when Char =/= $X -> 0;
		1 when Char =/= $M -> 0;
		2 when Char =/= $A -> 0;
		3 when Char =/= $S -> 0;
		_ -> traverse_dir(
			   Data,
			   LineLen,
			   Idx + dir_to_int(LineLen, Dir),
			   Dir,
			   Iter + 1
			  )
	end.

dir_to_int(LineLen, Dir) ->
	case Dir of
		w -> -1;
		e -> 1;
		n -> -LineLen;
		s -> LineLen;
		sw -> LineLen - 1;
		se -> LineLen + 1;
		nw -> -LineLen - 1;
		ne -> -LineLen + 1
	end.

