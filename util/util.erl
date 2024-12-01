-module(util).

-export([read_file/1]).

% @returns vector of string lines
read_file(FileName) ->
    {ok, IoDevice} = file:open(FileName, [read]),
    try read_lines(IoDevice)
      after file:close(IoDevice)
    end.

read_lines(IoDevice) ->
    case io:get_line(IoDevice, "") of
        eof  -> [];
        Line -> [Line | read_lines(IoDevice)]
    end.
