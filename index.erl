-module(index).
-export([get_file_contents/1,show_file_contents/1,index_lines/1,linenums_to_ranges/1,rangify_index/1]).

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


% to see full contents of the resulting index:
% io:format("~p~n", [Index])

%% paginate_index(Index) ->
%%     paginate_index(Index, []).

%% paginate_index([], PaginatedIndex) ->
%%     PaginatedIndex;
%% paginate_index([Entry|Entries], PaginatedIndex) ->
%%     paginate_index(Entries, [paginate_entry(Entry) | PaginatedIndex]).

rangify_index(Index) ->
    rangify_index(Index, []).

rangify_index([], RangifiedIndex) ->
    RangifiedIndex;
rangify_index([{Word, LineNums}|Entries], RangifiedIndex) ->
    rangify_index(Entries, [{Word, linenums_to_ranges(LineNums)} | RangifiedIndex]).

    

linenums_to_ranges(LineNums) ->
    linenums_to_ranges(LineNums, [], [], []).

linenums_to_ranges([], [Start|_], [End|_], Ranges) ->
    [{Start,End} | Ranges];
linenums_to_ranges([H|T], [], [], Ranges) ->
    linenums_to_ranges(T, [H], [H], Ranges);
linenums_to_ranges([H|T], [Start|_]=X, End, Ranges) when (H==Start) or (H==Start-1) ->
    linenums_to_ranges(T, [H | X], End, Ranges);
linenums_to_ranges([H|T], [Start|_], [End|_], Ranges) ->
    linenums_to_ranges(T, [H], [H], [{Start,End} | Ranges]).


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
    ((Char >= $A) and (Char =< $Z)) or ((Char >= $a) and (Char =< $z)).


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
