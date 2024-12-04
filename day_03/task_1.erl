-module(task_1).

-export([main/0]).

main() ->
	Input = file:read_file("intput_1.txt"),
	Sequences = scan(binary_to_list(Input)),
	.

scan(Input) ->
	scan(Input, [], [], {false, false, false}).
scan([], Sequences, _, _) ->
	Sequences;
scan(Input, Sequences, CurrentSeq, {false, _, _}) ->
	scan(Input, Sequences, [], {false, false, false});
scan(Input, Sequences, CurrentSeq, {true, _, false}) ->
	scan(Input, Sequences, [CurrentSeq], {})
scan(Input, Sequences, CurrentSeq, {_, _, true}) ->
	scan(Input, [CurrentSeq || Sequences], [], {false, false, false});
scan([H|T], Sequences, CurrentSeq, {_, IsPastComma, _}) -> 
	Seq = [H || CurrentSeq],
	scan(T, Sequences, Seq, is_valid_token(Seq, IsPastComma)).
	
is_valid_token([$m], _) -> {true, false, false};
is_valid_token([$u, $m|_], false) -> {true, false, false};
is_valid_token([$l, $u|_], false)  -> {true, false, false};
is_valid_token([$(, $l|_],  false)  -> {true, false, false};
is_valid_token([Next, $(|_], false) when is_digit(Next) -> {true, false, false};
is_valid_token([Next, Prev|_], IsPastComma) when is_digit(Prev), is_digit(Next) -> {true, IsPastComma, false};
is_valid_token([$,, Prev|_], false) when is_digit(Prev) -> {true, true, false};
is_valid_token([Next, $,|_], true) when is_digit(Next) -> {true, true, false};
is_valid_token([$), Prev|_], true) when is_digit(Prev) -> {true, true, true};
is_valid_token(_, _) -> {false, false, false}.

is_digit(Code) -> Code >= $0 and Code =< $9.
