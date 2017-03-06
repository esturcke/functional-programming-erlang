-module(week3).
-export([doubleAll/1, evens/1, product/1]).

-spec doubleAll([number()]) -> [number()].
doubleAll(Xs) -> lists:map(fun(X) -> X * 2 end, Xs).

-spec evens([integer()]) -> [integer()].
evens(Xs) -> lists:filter(fun(X) -> X rem 2 == 0 end, Xs).

-spec product([number()]) -> number().
product(Xs) -> lists:foldr(fun(A, B) -> A * B end, 1, Xs).