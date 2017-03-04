-module(index).
-export([get_file_contents/1,show_file_contents/1,index_lines/1]).

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

% TODO {"\\consecrated",[16]}
% TODO {"freedom","\e"}
% TODO {"government","\e"}
% TODO {"But","\r"},
% TODO {"larger","\r"},
% TODO {"sense","\r"},
% TODO {"dedicate","\r\b"},
% TODO {"should","\v"},
% TODO {"do","\v"},
% TODO {"might","\n"},
% TODO {"live","\n"},
% TODO {"altogether","\n"},
% TODO {"fitting","\n"},
% TODO {"proper","\n"},
% TODO {"resting","\t"},
% TODO {"place","\t"},
% TODO {"those","\t"},
% TODO {"their","\t"},
% TODO {"lives","\t"},
% TODO {"come","\b"},
% TODO {"portion","\b"},
% TODO {"as","\b"},
% TODO {"final","\b"},


% to see full contents of the resulting index:
% io:format("~p~n", [Index])

index_lines(Lines) ->
    index_lines(Lines, 1, []).

index_lines([], _LineNum, Index) ->
    Index;
index_lines([H|T], LineNum, Index) ->
    Words = split_into_words(H),
    index_lines(T, LineNum + 1, add_to_index(Index, Words, LineNum)).


split_into_words(Line) ->
    split_into_words(Line, []).

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


add_to_index(Index, [], _LineNum) ->
    Index;
add_to_index([], [Word|Words], LineNum) ->
    add_to_index([index_entry(Word, LineNum)], Words, LineNum);
add_to_index(Index, [Word|Words], LineNum) ->
    LineNums = search_index(Index, Word),
    IndexMinusWord = lists:delete({Word, LineNums}, Index),
    add_to_index([{Word, [LineNum | LineNums]} | IndexMinusWord], Words, LineNum).


search_index([], _Word) -> 
    [];
search_index([{Word,LineNums}|_T], Word) ->
    LineNums;
search_index([_H|T], Word) ->
    search_index(T, Word).


index_entry(Word, LineNum) ->
    {Word, [LineNum]}.
