-module(main).
-export([part1/1, part2/1]).

load_data(InputPath) ->
    case file:read_file(InputPath, [read, raw]) of
        {ok, FileText} -> FileText;
        {error, Error} -> erlang:error(Error)
    end.

parse_data(BinaryData) ->
    lists:unzip(
        lists:map(
            fun(X) ->
                [First | [Second | _]] = binary:split(X, [<<"   ">>]),
                {binary_to_integer(First), binary_to_integer(Second)}
            end,
            binary:split(BinaryData, [<<"\n">>], [global, trim])
        )
    ).

diff_data(TupleList) ->
    lists:map(
        fun({Left, Right}) -> abs(Left - Right) end,
        TupleList
    ).

part1(InputPath) ->
    {Left, Right} = parse_data(load_data(InputPath)),
    lists:sum(diff_data(lists:zip(lists:sort(Left), lists:sort(Right)))).

make_freq_dict([], Dict) ->
    Dict;
make_freq_dict([Head | Tail], Dict) ->
    NewDict = dict:update_counter(Head, 1, Dict),
    make_freq_dict(Tail, NewDict).

make_freq_dict(List) ->
    make_freq_dict(List, dict:new()).

freq_data(List, Freq) ->
    lists:map(
        fun(X) -> X * dict:fetch(X, Freq) end,
        lists:filter(fun(X) -> dict:is_key(X, Freq) end, List)
    ).

part2(InputPath) ->
    {Left, Right} = parse_data(load_data(InputPath)),
    lists:sum(freq_data(Left, make_freq_dict(Right))).
