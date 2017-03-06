-module(index).
-export([index/1]).

index(Lines) ->
    to_ranged_index(index_lines(Lines)).


% ------------------
% Base index builder
% ------------------

% Takes a list of strings (lines of text) as input, and indexes the content.
%
% Returns a list of index entries, where each entry is a word followed by a
% list of the line numbers that contain that word.  The line numbers will
% be in reverse order (last line first), and may contain duplicates.
% e.g. { "foo", [13,11,7,7,5,3] } indicates that the word "foo" occurs once on 
% lines 13,11,5 and 3, and twice on line 7.
index_lines(Lines) ->
    index_lines(Lines, 1, []).

index_lines([], _LineNum, Index) ->
    Index;
index_lines([First|Rest], LineNum, Index) ->
    Words = split_into_words(First),
    index_lines(Rest, LineNum + 1, add_to_index(Index, Words, LineNum)).


% Takes a single string and splits it into a list of words.  Any 'non-word
% character' will be treated as a delimeter.
%
% Returns a list of words.
% e.g. "one two three" will be split into ["one","two","three"].
split_into_words(Line) ->
    split_into_words(Line, []).

split_into_words([], Words) ->
    Words;
% we use lists:splitwith to assemble all leading 'word characters' into a word.
% 'non word characters' are treated as delimeters, and are discarded.
split_into_words([First|Rest]=Line, Words) ->
    case is_word_char(First) of
        true -> {Word, AfterWord} = lists:splitwith(fun is_word_char/1, Line),
                split_into_words(AfterWord, [Word | Words]);
        false -> split_into_words(Rest, Words)
    end.


% Returns true if Char is considered to be part of a word.
% 
% Currently only the characters A-Z and a-z are considered to be members
% of a word.  This implies that hypenated words such as 'battle-field' will
% be split into two words.  Similarly, abbreviations such as 'they're' or 
% 'e.g.' will be split into components.
is_word_char(Char) -> 
    ((Char >= $A) and (Char =< $Z)) or ((Char >= $a) and (Char =< $z)).


% Adds all of the supplied words to the index at the specified line number.
%
% Returns a new index that is based on the supplied index, but has all of
% specified words added at the specified line number.
% e.g. add_to_index([{"one", [1]}], ["one","two"], 42) will return
% [{"two", [42]},{"one", [42,1]}]
% Note that in this case, the existing entry for 'one' is modified, whereas
% a new entry is created for 'two'.
add_to_index(Index, [], _LineNum) ->
    Index;
add_to_index(Index, [Word|Words], LineNum) ->
    LineNums = linenums_for(Index, Word),
    IndexWithoutWord = lists:delete({Word, LineNums}, Index),
    add_to_index([{Word, [LineNum | LineNums]} | IndexWithoutWord], Words, LineNum).


% Looks up word in the supplied index, and returns the list of line numbers.
% Returns an empty list when the word is not found.
linenums_for([], _Word) ->
    [];
linenums_for([{Word,LineNums}|_Rest], Word) ->
    LineNums;
linenums_for([_First|Rest], Word) ->
    linenums_for(Rest, Word).


% -----------------------------------------
% Conversion of line numbers representation
% -----------------------------------------

% Takes an index containing entries where line numbers are represented as a simple
% list, and returns an index containing entries where line numbers are represented
% as a list of ranges.
% e.g. [{"foo", [11,3,2,1]}] becomes [{"foo", [{1,3},{11,11}]}]
to_ranged_index(Index) ->
    to_ranged_index(Index, []).

to_ranged_index([], RangedIndex) ->
    RangedIndex;
to_ranged_index([{Word, LineNums}|Rest], RangedIndex) ->
    to_ranged_index(Rest, [{Word, linenums_to_ranges(LineNums)} | RangedIndex]).

    
% Takes a list of line numbers and returns a list of line number ranges.
%
% The input list must be in reverse order (last line number first), and may
% contain duplicates.
% Returns an ordered (first line number first) set of line number ranges.
% e.g. the input [11,7,7,3,2,2,1] will generate the result [{1,3},{7,7},{11,11}]

linenums_to_ranges(LineNums) ->
    linenums_to_ranges(LineNums, [], [], []).

linenums_to_ranges([], [Start|_], [End|_], Ranges) ->
    [{Start,End} | Ranges];
linenums_to_ranges([H|T], [], [], Ranges) ->
    linenums_to_ranges(T, [H], [H], Ranges);
linenums_to_ranges([H|T], [Start|_]=S, End, Ranges) when (H == Start) or (H == Start - 1) ->
    linenums_to_ranges(T, [H | S], End, Ranges);
linenums_to_ranges([H|T], [Start|_], [End|_], Ranges) ->
    linenums_to_ranges(T, [H], [H], [{Start,End} | Ranges]).
