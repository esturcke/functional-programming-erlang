-module(week3).
-export([doubleAll/1, evens/1, product/1, zip/2, zip_with/3, zip_with_alt/3]).

-spec doubleAll([number()]) -> [number()].
doubleAll(Xs) -> lists:map(fun(X) -> X * 2 end, Xs).

-spec evens([integer()]) -> [integer()].
evens(Xs) -> lists:filter(fun(X) -> X rem 2 == 0 end, Xs).

-spec product([number()]) -> number().
product(Xs) -> lists:foldr(fun(A, B) -> A * B end, 1, Xs).

-spec zip([A], [B]) -> [{A, B}].
zip([], _) -> [];
zip(_, []) -> [];
zip([X|Xs], [Y|Ys]) -> [{X, Y}|zip(Xs, Ys)].

-spec zip_with(fun((X, Y) -> T), [X], [Y]) -> [T].
zip_with(_, [], _) -> [];
zip_with(_, _, []) -> [];
zip_with(F, [X|Xs], [Y|Ys]) -> [F(X, Y)|zip_with(F, Xs, Ys)].

-spec zip_with_alt(fun((X, Y) -> T), [X], [Y]) -> [T].
zip_with_alt(F, Xs, Ys) -> lists:map(fun({X, Y}) -> F(X, Y) end, zip(Xs, Ys)).