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
  Words =split(Lines), 
  Words.

% Zip a list with the index in the for each item in the list
-spec number([string()]) -> [{string(), integer()}].
number(Lines) -> lists:zip(Lines, lists:seq(1, length(Lines))).

% Split each numbered line into words
-spec split([{string(), integer()}]) -> [{string(), integer()}].
split(Lines) -> lists:flatmap(fun({Line, Index}) -> lists:map(fun(Word) -> {Word, Index} end, words(Line)) end, Lines).

% Split a line into words
-spec words(string()) -> [string()].
words(Line) -> string:tokens(string:to_lower(Line), " ").

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

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
  io:format("~s~n",[L]),
  show_file_contents(Ls);
show_file_contents([]) ->
  ok.