-module(bits).
-export([bits/1, bits2/1]).

% non-tail recursive bit counter
bits(0) ->
    0;

bits(X) when X > 0, X rem 2 == 1 ->
    1 + bits(X div 2);

bits(X) when X > 0 ->
    bits(X div 2).

% tail recursive bit counter
bits2(X) when X >= 0 ->
    bits2(X, 0).

bits2(0, A) ->
    A;

bits2(X, A) when X rem 2 == 1 ->
    bits2(X div 2, A + 1);

bits2(X, A) ->
    bits2(X div 2, A).
