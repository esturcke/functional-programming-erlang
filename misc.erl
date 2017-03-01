-module(misc).
-export([xOrOne/2, xOrTwo/2, xOrThree/2, maxThree/3, howManyEqual/3, fib/1, pieces/1, fibFast/1, perfect/1, product/1, maximum/1, double/1, evens/1, take/2, nub/1, palindrome/1, join/2, concat/1, member/2, merge_sort/1, quick_sort/1, insertion_sort/1, perms/1]).

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

-spec take(integer(), [T]) -> [T].
take(N, Xs) when N >= 0 -> take(N, Xs, []).
take(0, _, Ys) -> reverse(Ys);
take(_, [], Ys) -> reverse(Ys);
take(N, [X|Xs], Ys) -> take(N - 1, Xs, [X|Ys]).

-spec in(T, [T]) -> boolean.
in(_, []) -> false;
in(X, [X|_]) -> true;
in(X, [_|Ys]) -> in(X, Ys).

-spec nub([T]) -> [T].
nub(X) -> nub(X, []).
nub([], Ys) -> reverse(Ys);
nub([X|Xs], Ys) -> nub(Xs, case in(X, Ys) of true -> Ys; false -> [X|Ys] end).

% convert to lower case, stripping out anything that's not a letter
-spec lower_case_letters(string) -> string.
lower_case_letters(S) -> lists:filtermap(fun(X) when $a =< X, X =< $z -> true;
                                            (X) when $A =< X, X =< $Z -> {true, X - $A + $a};
                                            (_) -> false end, S).

-spec palindrome(string) -> boolean.
palindrome(S) ->
    S2 = lower_case_letters(S),
    S2 == lists:reverse(S2).

-spec join([T], [T]) -> [T].
join(As, Bs) -> reverse_join(reverse(As), Bs).

% tail recursive joining of the reverse of the first list to the second
-spec reverse_join([T], [T]) -> [T].
reverse_join([], Bs) -> Bs;
reverse_join([A|As], Bs) -> reverse_join(As, [A|Bs]). 

-spec concat([[T]]) -> [T].
concat([]) -> [];
concat([A]) -> A;
concat([A1,A2|As]) -> concat([join(A1, A2)|As]).

-spec member(T, [T]) -> boolean.
member(A, Bs) -> in(A, Bs).

-spec merge_sort([T]) -> [T].
merge_sort([]) -> [];
merge_sort([A]) -> [A];
merge_sort(Xs) ->
    {As, Bs} = half(Xs),
    merge_sorted(merge_sort(As), merge_sort(Bs)).

-spec merge_sorted([T], [T]) -> [T].
merge_sorted([], Bs) -> Bs;
merge_sorted(As, []) -> As;
merge_sorted([A|As], Bs = [B|_]) when A < B -> [A|merge_sorted(As, Bs)];
merge_sorted(As, [B|Bs]) -> [B|merge_sorted(As, Bs)].

-spec half([T]) -> {[T], [T]}.
half([]) -> {[], []};
half([A]) -> {[A], []};
half([A1,A2|As]) ->
    {A1s, A2s} = half(As),
    {[A1|A1s], [A2|A2s]}.

-spec quick_sort([T]) -> [T].
quick_sort([]) -> [];
quick_sort([A|As]) ->
    {Less, More} = pivot(A, As),
    concat([quick_sort(Less), [A], quick_sort(More)]).

-spec pivot(T, [T]) -> {[T], [T]}.
pivot(_, []) -> {[], []};
pivot(A, [B|Bs]) when B < A ->
    {Less, More} = pivot(A, Bs),
    {[B|Less], More};
pivot(A, [B|Bs]) ->
    {Less, More} = pivot(A, Bs),
    {Less, [B|More]}.

-spec insertion_sort([T]) -> [T].
insertion_sort([]) -> [];
insertion_sort([A|As]) -> insert_sorted(A, insertion_sort(As)).

-spec insert_sorted(T, [T]) -> [T].
insert_sorted(A, []) -> [A];
insert_sorted(A, [B|Bs]) when B < A -> [B|insert_sorted(A, Bs)];
insert_sorted(A, Bs) -> [A|Bs].

-spec perms([T]) -> [[T]].
perms([]) -> [[]];
perms([A|As]) -> insert_in_each(A, perms(As)).

-spec insert_in_each(T, [[T]]) -> [[T]].
insert_in_each(_, []) -> [];
insert_in_each(A, [B|Bs]) -> concat([insert_everywhere(A, B), insert_in_each(A, Bs)]).


-spec insert_everywhere(T, [T]) -> [[T]].
insert_everywhere(A, []) -> [[A]];
insert_everywhere(A, [B]) -> [[A,B], [B,A]];
insert_everywhere(A, [B|Bs]) -> [[A,B|Bs]|prepend_lists(B,insert_everywhere(A, Bs))].

% prepend a value to each list
-spec prepend_lists(T, [[T]]) -> [[T]].
prepend_lists(_, []) -> [];
prepend_lists(A, [B|Bs]) -> [[A|B]|prepend_lists(A, Bs)].
