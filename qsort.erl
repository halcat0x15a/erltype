qsort0([]) -> [];
qsort0([Pivot | Rest]) ->
    qsort0([X || X <- Rest, X < Pivot]) ++ [Pivot] ++ qsort0([Y || Y <- Rest, Y >= Pivot]).

qsort([], _) -> [];
qsort([Pivot|Rest], Smaller) ->
    qsort([X || X <- Rest, Smaller(X, Pivot)], Smaller)
    ++ [Pivot] ++
    qsort([Y || Y <- Rest, not(Smaller(Y, Pivot))], Smaller).
