-module(rps).
-export([tournament/2]).

% Rock-paper-scissors
-type move() :: rock | paper | scissors.
-type result() :: win | lose | draw.
-type round() :: {move(), move()}.

% Return the move that beats the argument
-spec beats(move()) -> move().
beats(rock) -> paper;
beats(paper) -> scissors;
beats(scissors) -> rock.

% Given a round, what is the outcome for the left player
-spec left_result(round()) -> result().
left_result({A, A}) -> tie;
left_result({A, B}) -> case A =:= beats(B) of
   true -> win;
   false -> lose
end.

% For a sequence of moves, what is the net win count for the left player
-spec tournament([move()], [move()]) -> integer().
tournament(LeftMoves, RightMoves) -> lists:foldr(fun(Round, Sum) -> case left_result(Round) of
  win -> Sum + 1;
  lose -> Sum - 1;
  tie -> Sum
end end, 0, lists:zip(LeftMoves, RightMoves)).