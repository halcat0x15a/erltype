qsort([], _) -> [];
qsort([Pivot|Rest], Smaller) ->
    qsort([X || X <- Rest, Smaller(X, Pivot)], Smaller)
    ++ [Pivot] ++
    qsort([Y || Y <- Rest, not(Smaller(Y, Pivot))], Smaller).

by_length(Lists) ->
    F = fun(A, B) when is_list(A), is_list(B) ->
      length(A) < length(B)
    end,
    qsort(Lists, F).
