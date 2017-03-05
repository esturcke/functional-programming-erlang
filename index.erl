-module(index).
-export([create/1]).

% Given a file name, returns the sorted index with
% entries that look like
%
%   { "foo" , [{3,5},{7,7},{11,13}] }
%
-spec create(string()) -> [{string(), [{integer(), integer()}]}].
create(Name) ->
  Lines = number(get_file_contents(Name)),
  Words = group(sort(split(Lines))), 
  Words.

% Zip a list with the index in the for each item in the list
-spec number([string()]) -> [{string(), integer()}].
number(Lines) -> lists:zip(Lines, lists:seq(1, length(Lines))).

% Split each numbered line into words
-spec split([{string(), integer()}]) -> [{string(), integer()}].
split(Lines) -> lists:flatmap(fun({Line, Index}) -> lists:map(fun(Word) -> {Word, Index} end, words(Line)) end, Lines).

% Split a line into words
-spec words(string()) -> [string()].
words(Line) -> normalize(string:tokens((Line), " ")).

% Remove non-words and normalize spelling
-spec normalize([string()]) -> [string()].
normalize(Words) -> lists:filtermap(fun(Word) ->
                                      case valid(Word) of
                                        true -> { true, string:to_lower(Word) };
                                        false -> false
                                      end
                                    end, Words).

% Check if a word should be included
-spec valid(string()) -> boolean().
valid(Word) -> lists:all(fun(Letter) when $a =< Letter, Letter =< $z -> true;
                            (Letter) when $A =< Letter, Letter =< $Z -> true;
                            ($') -> true;
                            (_) -> false end, Word).

% Sort word/line numebr pairs by word
-spec sort([{string(), integer()}]) -> [{string(), integer()}].
sort(Words) -> lists:sort(fun({WordA, _}, {WordB, _}) -> WordA =< WordB end, Words).

% Group identical words and form 
-spec group([{string(), integer()}]) -> [{string(), [{integer(), integer()}]}].
group(A) -> A.

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof ->
      file:close(File),
      Partial;
    Line ->
      {Strip,_} = lists:split(length(Line)-1,Line),
      get_all_lines(File,[Strip|Partial])
    end.