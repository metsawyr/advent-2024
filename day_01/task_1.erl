-module(task_1).

-import(util, [read_file/1]).

-export([main/0]).

main() ->
	Lines = read_file("input_1.txt"),
	{Left, Right} = parse_input(Lines),
	sum_distances(bin_sort(Left), bin_sort(Right)).

parse_input(Lines) ->
	parse_input(Lines, [], []).
parse_input([], Left, Right) ->
	{Left, Right};
parse_input([Head|Tail], Left, Right) ->
	{LNumber, RNumber} = extract_numbers(Head),
	parse_input(Tail, [LNumber | Left], [RNumber | Right]).

extract_numbers(Line) ->
	extract_numbers(Line, 0, 0, false).
extract_numbers([], Left, Right, _) ->
	{Left, Right};
extract_numbers([Head|Tail], Left, Right, _) when Head - $0 < 0; Head - $0 > 9 ->
	extract_numbers(Tail, Left, Right, true);
extract_numbers([Head|Tail], Left, Right, IsLeftComplete) -> 
	case IsLeftComplete of
		true -> 
			extract_numbers(Tail, Left, Right*10+Head-$0, IsLeftComplete);
		false -> 
			extract_numbers(Tail, Left*10+Head-$0, Right, IsLeftComplete)
	end.

sum_distances(Left, Right) ->
	sum_distances(Left, Right, 0).
sum_distances([], [], Sum) -> 
	Sum;
sum_distances([LHead|LTail], [RHead|RTail], Sum) -> 
	sum_distances(LTail, RTail, Sum + abs(LHead-RHead)).

bin_sort(List) -> 
	bin_sort(List, []).
bin_sort([], Sorted) -> 
	Sorted;
bin_sort([Head | Tail], Sorted) ->
	bin_sort(Tail, insert_sorted(Sorted, Head)).

insert_sorted(Sorted, Element) -> 
	insert_sorted(Sorted, Element, 0, length(Sorted)).

insert_sorted([], Element, _, _) -> 
	[Element];  
insert_sorted(Sorted, Element, Low, High) when Low >= High -> 
	lists:sublist(Sorted, 1, Low) ++ [Element] ++ lists:sublist(Sorted, Low + 1, length(Sorted) - Low);
insert_sorted(Sorted, Element, Low, High) ->
	Mid = (Low + High) div 2,
	MidElement = lists:nth(Mid + 1, Sorted),
	case Element of
		X when X < MidElement -> insert_sorted(Sorted, Element, Low, Mid);
		X when X >= MidElement -> insert_sorted(Sorted, Element, Mid + 1, High)
	end.
