qsort([], _) -> [];
qsort([Pivot|Rest], Smaller) ->
    qsort([X || X <- Rest, Smaller(X, Pivot)], Smaller)
    ++ [Pivot] ++
    qsort([Y || Y <- Rest, not(Smaller(Y, Pivot))], Smaller).
