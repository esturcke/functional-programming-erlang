-module(misc).
-export([xOrOne/2, xOrTwo/2, xOrThree/2, maxThree/3, howManyEqual/3, fib/1, pieces/1, fibFast/1, perfect/1, product/1, maximum/1, double/1, evens/1]).

xOrOne(X, Y) ->
    X =/= Y.

xOrTwo(X, Y) ->
    X == not Y.

xOrThree(X, Y) ->
    not (X == Y).

maxThree(A, B, C) ->
    max(max(A, B), C).

howManyEqual(A, A, A) ->
    3;
howManyEqual(A, A, _) ->
    2;
howManyEqual(A, _, A) ->
    2;
howManyEqual(_, A, A) ->
    2;
howManyEqual(_, _, _) ->
    0.

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when N > 1 ->
    fib(N - 2) + fib(N - 1).

pieces(0) ->
    1;
pieces(N) when N > 0 ->
    pieces(N - 1) + N.

fibFast(N) when N >= 0 ->
    fibFast(N, 0, 1).

fibFast(0, A, _) ->
    A;
fibFast(1, _, B) ->
    B;
fibFast(N, A, B) ->
    fibFast(N - 1, B, A + B).

perfect(1) ->
    false;
perfect(N) when N > 1 ->
    perfect(N, N - 1, 0).

perfect(N, 1, A) ->
    N == A + 1;
perfect(N, I, A) when N rem I == 0 ->
    perfect(N, I - 1, A + I);
perfect(N, I, A) ->
    perfect(N, I - 1, A).

product(Xs) -> product(Xs, 1).
product([], A) -> A;
product([X|Xs], A) -> product(Xs, A * X).

maximum([X]) -> X;
maximum([X|Xs]) -> maximum(Xs, X).
maximum([X], M) -> max(X, M);
maximum([X|Xs], M) -> maximum(Xs, max(X, M)).

reverse(Xs) -> reverse(Xs, []).
reverse([], Ys) -> Ys;
reverse([X|Xs], Ys) -> reverse(Xs, [X|Ys]).

% Transforming list elements
% Define an Erlang function double/1 to double the elements of a list of numbers.
double(Xs) -> double(Xs, []).
double([], Ys) -> reverse(Ys);
double([X|Xs], Ys) -> double(Xs, [2 * X|Ys]).

% Filtering lists
% Define a function evens/1 that extracts the even numbers from a list of integers.
evens(Xs) -> evens(Xs, []).
evens([], Ys) -> reverse(Ys);
evens([X|Xs], Ys) when X rem 2 == 0 -> evens(Xs, [X|Ys]);
evens([_|Xs], Ys) -> evens(Xs, Ys).
