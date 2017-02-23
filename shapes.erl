-module(shapes).
-export([perimeter/1, area/1, enclose/1]).

perimeter({triangle, {A, B, C}}) ->
    A + B + C;

perimeter({square, {A}}) ->
    4 * A;

perimeter({rectangle, {A, B}}) ->
    2 * (A + B).

area({triangle, {A, B, C}}) when A + B > C, A + C > B, B + C > A ->
    S = (A + B + C) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C));

area({square, {A}}) ->
    A * A;

area({rectangle, {A, B}}) ->
    A * B.

enclose(S = {triangle, {A, _, _}}) ->
    {rectangle, {A, 2 * area(S) / A}};

enclose({square, {A}}) ->
    {rectangle, {A, A}};

enclose(S = {rectangle, _}) ->
    S.
