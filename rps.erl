-module(rps).
-export([play/1, play_two/3, echo/1, rock/1, no_repeat/1, cycle/1, rand/1, least_frequent/1, most_frequent/1, tournament/2]).

% Rock-paper-scissors
-type move() :: rock | paper | scissors.
-type result() :: win | lose | draw.
-type round() :: {move(), move()}.
-type moves() :: [move()].

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
tournament(LeftMoves, RightMoves) ->
  Rounds = lists:zip(LeftMoves, RightMoves),
  lists:foldr(fun(Round, Sum) -> case left_result(Round) of
    win -> Sum + 1;
    lose -> Sum - 1;
    tie -> Sum
  end end, 0, Rounds).

%
% play one strategy against another, for N moves.
%

play_two(StrategyL, StrategyR, N) ->
    play_two(StrategyL, StrategyR, [] ,[] ,N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

play_two(_,_,PlaysL,PlaysR,0) ->
   dummy;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   dummy.

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");
	_    ->
	    Result = left_result({Play, Strategy(Moves)}),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% pick a random move
-spec pick() -> move().
pick() -> pick([rock, paper, scissors]).

% pick from a list of moves
-spec pick(moves()) -> move().
pick(Moves) -> lists:nth(rand:uniform(length(Moves)), Moves).

% count history rock/paper/scissors plays in a {rock, paper, scissors} count tuple
-spec counts(moves()) -> {integer(), integer(), integer()}.
counts(Moves) ->
  F = fun(rock, {R, P, S}) -> {R + 1, P, S};
         (paper, {R, P, S}) -> {R, P + 1, S};
         (scissors, {R, P, S}) -> {R, P, S + 1} end,
  Counts = lists:foldr(F, {0, 0, 0}, Moves),
  io:format("~w~n", [Counts]),
  Counts.

% given the count tuple, get a list of most frequent moves
-spec frequent({integer(), integer(), integer()}) -> [move()].
frequent({X, X, X}) -> [rock, paper, scissors];
frequent({X, X, Y}) when X > Y -> [rock, paper];
frequent({X, Y, X}) when X > Y -> [rock, scissors];
frequent({Y, X, X}) when X > Y -> [paper, scissors];
frequent({X, Y, Z}) when X > Y, X > Z -> [rock];
frequent({X, Y, Z}) when Y > X, Y > Z -> [paper];
frequent({X, Y, Z}) when Z > X, Z > Y -> [scissors].

% given the count tuple, get a list of least frequent moves
-spec infrequent({integer(), integer(), integer()}) -> [move()].
infrequent({X, X, X}) -> [rock, paper, scissors];
infrequent({X, X, Y}) when X < Y -> [rock, paper];
infrequent({X, Y, X}) when X < Y -> [rock, scissors];
infrequent({Y, X, X}) when X < Y -> [paper, scissors];
infrequent({X, Y, Z}) when X < Y, X < Z -> [rock];
infrequent({X, Y, Z}) when Y < X, Y < Z -> [paper];
infrequent({X, Y, Z}) when Z < X, Z < Y -> [scissors].

%
% strategies.
%
-spec echo(moves()) -> move().
echo([]) -> paper;
echo([Last|_]) -> Last.

-spec rock(moves()) -> move().
rock(_) -> rock.

-spec no_repeat(moves()) -> move().
no_repeat([]) -> pick();
no_repeat([rock|_]) -> scissors;
no_repeat([scissors|_]) -> paper;
no_repeat([paper|_]) -> rock.

-spec cycle(moves()) -> move().
cycle(Xs) -> lists:nth(1 + length(Xs) rem 3, [rock, paper, scissors]).

-spec rand(moves()) -> move().
rand(_) -> pick().

-spec least_frequent(moves()) -> move().
least_frequent(Moves) -> beats(pick(infrequent(counts(Moves)))).

-spec most_frequent(moves()) -> move().
most_frequent(Moves) -> beats(pick(frequent(counts(Moves)))).