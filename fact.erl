fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

test() -> [1] ++ [2], ["hoge"] ++ ["fuga"].