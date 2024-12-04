-module(main).
-export([part1/1, part2/1]).

load_data(InputPath) ->
    case file:read_file(InputPath, [read, raw]) of
        {ok, FileText} -> FileText;
        {error, Error} -> erlang:error(Error)
    end.

valid_muls(String) ->
    {ok, Re} = re:compile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"),
    {match, Matches} = re:run(String, Re, [global]),
    Matches.

parse_muls(Data, Matches) ->
    lists:map(
        fun(Match) ->
            [_, {Aidx, Alen}, {Bidx, Blen}] = Match,
            A = list_to_integer(lists:sublist(Data, 1 + Aidx, Alen)),
            B = list_to_integer(lists:sublist(Data, 1 + Bidx, Blen)),
            A * B
        end,
        Matches
    ).

part1(InputPath) ->
    Data = binary:bin_to_list(load_data(InputPath)),
    Matches = valid_muls(Data),
    lists:sum(parse_muls(Data, Matches)).

do_idxs(String) ->
    {ok, Re} = re:compile("do\\(\\)"),
    {match, Matches} = re:run(String, Re, [global]),
    lists:map(fun([{Idx, _}]) -> Idx end, Matches).

dont_idxs(String) ->
    {ok, Re} = re:compile("don't\\(\\)"),
    {match, Matches} = re:run(String, Re, [global]),
    lists:map(fun([{Idx, _}]) -> Idx end, Matches).

make_range_tree(Dos, Donts) ->
    Tree = lists:foldl(
        fun(X, Acc) -> gb_trees:insert(X, true, Acc) end,
        gb_trees:empty(),
        Dos
    ),
    lists:foldl(
        fun(X, Acc) -> gb_trees:insert(X, false, Acc) end,
        Tree,
        Donts
    ).

enabled_muls(Matches, RangeTree) ->
    lists:filter(
        fun([{Idx, _}, _, _]) ->
            case gb_trees:smaller(Idx, RangeTree) of
                none -> true;
                {_, Should} -> Should
            end
        end,
        Matches
    ).

part2(InputPath) ->
    Data = binary:bin_to_list(load_data(InputPath)),
    RangeTree = make_range_tree(do_idxs(Data), dont_idxs(Data)),
    Matches = enabled_muls(valid_muls(Data), RangeTree),
    lists:sum(parse_muls(Data, Matches)).
