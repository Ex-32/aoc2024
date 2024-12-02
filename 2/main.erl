-module(main).
-export([part1/1, part2/1]).

load_data(InputPath) ->
    case file:read_file(InputPath, [read, raw]) of
        {ok, FileText} -> FileText;
        {error, Error} -> erlang:error(Error)
    end.

parse_data(BinaryData) ->
    lists:map(
        fun(X) ->
            lists:map(
                fun(Y) ->
                    binary_to_integer(Y)
                end,
                binary:split(X, [<<" ">>], [global])
            )
        end,
        binary:split(BinaryData, [<<"\n">>], [global, trim])
    ).

is_safe_inc([A | [B | Tail]]) ->
    Diff = abs(A - B),
    Diff =< 3 andalso Diff > 0 andalso B > A andalso is_safe_inc([B | Tail]);
is_safe_inc([_]) ->
    true.

is_safe_dec([A | [B | Tail]]) ->
    Diff = abs(A - B),
    Diff =< 3 andalso Diff > 0 andalso B < A andalso is_safe_dec([B | Tail]);
is_safe_dec([_]) ->
    true.

is_safe([First | [Second | Tail]]) when First < Second ->
    is_safe_inc([First | [Second | Tail]]);
is_safe([First | [Second | Tail]]) when First > Second ->
    is_safe_dec([First | [Second | Tail]]);
is_safe(_) ->
    false.

part1(InputPath) ->
    lists:foldl(
        fun(Report, Sum) ->
            case is_safe(Report) of
                true -> Sum + 1;
                false -> Sum
            end
        end,
        0,
        parse_data(load_data(InputPath))
    ).

withoutnth(N, List) ->
    {Left, Right} = lists:split(N, List),
    lists:droplast(Left) ++ Right.

possibles(List) ->
    Range = lists:seq(1, length(List)),
    SubLists = lists:map(fun(I) -> withoutnth(I, List) end, Range),
    [List | SubLists].


part2(InputPath) ->
    lists:foldl(
        fun(Report, Sum) ->
            case lists:any(fun(X) -> is_safe(X) end, possibles(Report)) of
                true -> Sum + 1;
                false -> Sum
            end
        end,
        0,
        parse_data(load_data(InputPath))
    ).
