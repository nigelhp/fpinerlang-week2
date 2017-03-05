-module(file_index).
-export([index/1]).

% Index the content of the text file specified by Name.
%
% Returns a list of entries, where each entry is a word followed
% by a list of the ranges of lines which contained that word.
% For example, the entry: { "foo" , [{3,5},{7,7},{11,13}] } means 
% that the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the
% file.
%
% Note that to see the details of the returned index, you can use 
% 'io:format("~p~n", [ResultOfThisFunction])'
index(Name) ->
    index:index(get_file_contents(Name)).


% --------------------------------------------
% Provided functions (originally in index.erl)
% --------------------------------------------

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
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.
