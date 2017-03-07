-module(rps).
-export([tournament/2]).

% Rock-paper-scissors
-type move() :: rock | paper | scissors.
-type result() :: win | lose | draw.
-type round() :: {move(), move()}.

-spec beat(move()) -> move().
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

-spec lose(move()) -> move().
lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.

-spec left_result(round()) -> result().
left_result({A, A}) -> tie;
left_result({A, B}) -> case A =:= beat(B) of
   true -> win;
   false -> lose
end.

-spec tournament([move()], [move()]) -> integer().
tournament(LeftMoves, RightMoves) -> lists:foldr(fun(Round, Sum) -> case left_result(Round) of
  win -> Sum + 1;
  lose -> Sum - 1;
  tie -> Sum
end end, 0, lists:zip(LeftMoves, RightMoves)).