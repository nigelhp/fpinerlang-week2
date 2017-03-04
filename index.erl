-module(index).
-export([get_file_contents/1,show_file_contents/1,split_into_words/2]).

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

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    


% BEGIN ...

split_into_words([], Words) ->
    Words;
split_into_words([H|T]=Chars, Words) ->
    case is_word_char(H) of
        true -> {Word, Remaining} = lists:splitwith(fun (C) -> is_word_char(C) end, Chars),
                split_into_words(Remaining, [Word | Words]);
        false -> split_into_words(T, Words)
    end.


is_word_char(Char) -> 
    (Char >= $A) and (Char =< $z).
