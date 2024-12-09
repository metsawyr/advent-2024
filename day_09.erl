-module(day_09).

-export([main/1]).

main(File) ->
	{ok, Input} = file:read_file(File),
	LastIndex = byte_size(Input) - 1,
	calculate_checksum(Input, 0, LastIndex).

calculate_checksum(Input, ForwardIter, ReverseIter) ->
	calculate_checksum(Input, ForwardIter, ReverseIter, 0, 0, []).
calculate_checksum(Input, ForwardIter, ReverseIter, TotalLength, Checksum, BackBuffer) when ForwardIter >= ReverseIter ->
	BlockSize = block_size(Input, ForwardIter),	
	Checksum + derive_partial_checksum(ForwardIter div 2, TotalLength, BlockSize) + buffer_checksum(BackBuffer, TotalLength + BlockSize);
calculate_checksum(Input, ForwardIter, ReverseIter, TotalLength, Checksum, BackBuffer) ->
	BlockSize = block_size(Input, ForwardIter),	
	case ForwardIter rem 2 of
		0 -> 
			calculate_checksum(
				Input,
				ForwardIter+1,
				ReverseIter,
				TotalLength + BlockSize,
				Checksum + derive_partial_checksum(ForwardIter div 2, TotalLength, BlockSize),
				BackBuffer
			 );
		1 when (ReverseIter rem 2) == 0 -> 
			BufferSize = lists:foldl(fun({BufBlockSize, _}, Acc) -> Acc + BufBlockSize end, 0, BackBuffer),
			{NewRevIter, BufExtension} = fetch_buffer(Input, BlockSize - BufferSize, ReverseIter),
			{NewBuffer, Drainage} = drain_buffer(BackBuffer ++ BufExtension, BlockSize),
			PartialChecksum = buffer_checksum(Drainage, TotalLength),

			calculate_checksum(
				Input,
				ForwardIter+1,
				NewRevIter,
				TotalLength + BlockSize,
				Checksum + PartialChecksum,
				NewBuffer
			 );
		_ ->
			calculate_checksum(
				Input,
				ForwardIter,
				ReverseIter-1,
				TotalLength,
				Checksum,
				BackBuffer
			 )
	end.

fetch_buffer(Input, RequiredSize, ReverseIter) ->
	fetch_buffer(Input, RequiredSize, ReverseIter, []).
fetch_buffer(_, RequiredSize, ReverseIter, Buffer) when RequiredSize < 1 ->
	{ReverseIter, lists:reverse(Buffer)};
fetch_buffer(Input, RequiredSize, ReverseIter, Buffer) -> 
	BlockSize = block_size(Input, ReverseIter),
	fetch_buffer(
		Input, 
	  	RequiredSize-BlockSize,
		ReverseIter-2,
		[{BlockSize, ReverseIter div 2} | Buffer]
	 ).

drain_buffer(Buf, FetchSize) ->
	drain_buffer(Buf, FetchSize, []).
drain_buffer(Buf, FetchSize, Drainage) when FetchSize < 1 ->
	{Buf, lists:reverse(Drainage)};
drain_buffer([{BlockSize, Index} = H | T], FetchSize, Drainage) ->
	case BlockSize - FetchSize of
		SizeDelta when SizeDelta > 0 ->
			drain_buffer([{SizeDelta, Index} | T], 0, [{FetchSize, Index} | Drainage]);
		_ ->
			drain_buffer(T, FetchSize - BlockSize, [H | Drainage])
		end.

derive_partial_checksum(Value, Index, Iterations) ->
	derive_partial_checksum(Value, Index, Iterations, 0).
derive_partial_checksum(_, _, 0, Checksum) ->
	Checksum;
derive_partial_checksum(Value, Index, Iterations, Checksum) ->
	derive_partial_checksum(Value, Index+1, Iterations-1, Checksum + Value*Index).

buffer_checksum(Buffer, Index) ->
	{Checksum, _} = lists:foldl(
	  	fun({BlockSize, Value}, {Sum, Len}) -> {Sum + derive_partial_checksum(Value, Len, BlockSize), Len + BlockSize} end,
	  	{0, Index}, 
	  	Buffer
	  ),

	Checksum.

block_size(Input, Index) -> binary:at(Input, Index) - $0.

