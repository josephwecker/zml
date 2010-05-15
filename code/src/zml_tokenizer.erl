%% Takes a zml input file and tokenizes it for a parser.
%% Should handle unicode just fine.
%%
%% AUTHOR: Joseph Wecker <joseph.wecker@gmail.com>
%%
%%
%% TODO:
%%   - Problem with spaces when inline tags are _way_ inline.  Might be more of
%%     a parser problem...  "bol:strong ogna;-is-my-first-name" needs to be
%%     rendered without any spaces, for example.
%%   - Error currently if entire file is indented.  i.e., first line isn't
%%     currently allowed to be indented.  This is a problem- needs to be
%%     factored out.
%%   - Figure out either here or in the parser how to handle normal text that
%%     has a hanging indent.
%%
%% EVENTUALLY:
%%   - Use macros or something to improve readability of the main tokenizing
%%     loops.
%%
%% NOTES:
%%   - I might just let the parser decide when to ignore false
%%     "start/end_attrs" tokens
%%   - Template encoding ("[...]") will be handled post-parser probably
%%

-module(zml_tokenizer).

-include("zml_tokenizer.hrl").

-export([tokenize_stream/1, tokenize_file/1, tokenize_string/1]).


tokenize_file(Filename) when is_list(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  tokenize_stream(File).

tokenize_stream(Stream) ->
  erase(),
  put(line_num, 0),
  % Just ignore leading "indent" token if needed
  parse_lines(fun() -> io:get_line(Stream, "") end, [0], [], false).

tokenize_string(InStr) ->
  erase(),
  put(line_num, 0),
  put('--input-string--', InStr),
  parse_lines(fun string_feed/0, [0], [], false).

% Fake string equivalent of io:get_line
string_feed() ->
  Str = get('--input-string--'),
  {Res, NewStr} =
    case Str of
      "" -> {eof, ""};
      _ ->
        case string:chr(Str, $\n) of
          0 -> {Str, ""};
          Idx -> lists:split(Idx, Str)
        end
    end,
  put('--input-string--', NewStr),
  Res.

parse_lines(Next, IndentStack, RTokens, InAttr) ->
  put(line_num, get(line_num) + 1),
  put(next_fun, Next),
  case Next() of
    eof ->
      LN = get(line_num) - 1,
      Tokens2 = lists:reverse(lists:flatten(RTokens)),
      ExtraDedents = lists:map(fun(_) -> {dedent, LN} end,
        lists:seq(1,length(IndentStack) - 1)),
      Tokens2 ++ ExtraDedents ++ [{end_of_file, LN}];
    {error, Reason} ->
      erlang:error({input_zml_file_read_error, Reason});
    Line ->
      {NumDents, RemainingLine} = get_dent(Line),
      case RemainingLine of
        [] -> % Blank line
          parse_lines(Next, IndentStack, RTokens, InAttr);
        _ ->
          {NewStack, MoreTokens, InAttr2} =
            parse_line(NumDents, IndentStack, RemainingLine, InAttr),
          case MoreTokens of
            [] ->
              parse_lines(Next, NewStack, RTokens, InAttr2);
            _ ->
              parse_lines(Next, NewStack, [MoreTokens | RTokens], InAttr2)
          end
      end
  end.

%% How many whitespace characters lead up the line.
%% (Calculated with unicode characters- that's why it looks funny)
get_dent(Line) ->
  get_dent(Line, 0).
get_dent([H|T],N) when ?IS_WHITESPACE(H) ->
  get_dent(T,N+1);
get_dent(Rest,N) ->
  {N, Rest}.

%% Figures out indent / dedent tokens and then calls parse_inner to figure out
%% the rest of whatever is there.  Skips indent/outdent if currently processing
%% attributes.
parse_line(Dents, CStack, Line, InAttr) ->
  {ParsedLine, InAttr2} = parse_inner(Line, InAttr),
  case ParsedLine of
    [] ->
      % Blank lines (after comments removed etc.) are ignored
      {CStack, [], InAttr};
    _ ->
      % Newline appended if we're not still in the middle of attributes
      AlteredPLine = 
        case InAttr2 of
          true -> ParsedLine;
          false -> [{newline, get(line_num)} | ParsedLine]
        end,
      % Dedents/indent prepended if we didn't start inside of attributes
      case InAttr of
        true -> {CStack, AlteredPLine, InAttr2};
        false ->
          {CS2, APL2} = do_parse_line(Dents, CStack, AlteredPLine),
          {CS2, APL2, InAttr2}
      end
  end.

do_parse_line(Dents, [NLast | _] = CStack, PLine) when Dents == NLast ->
  {CStack, PLine};
do_parse_line(Dents, [NLast | _] = CStack, PLine) when Dents > NLast ->
  {[Dents | CStack], [PLine, {indent, get(line_num)}]};
do_parse_line(Dents, CStack, PLine) ->
  {NewStack, DentTokens} = do_dedent(Dents, CStack, []),
  {NewStack, [PLine, DentTokens]}.

%% Pops indents off the stack until it matches the current indent- issuing a
%% "dedent" for each one.
%% Throws an error if the current indent is not aligned with some past
%% indentation level.
do_dedent(_, [], TokenAcc) ->
  {[], TokenAcc};
do_dedent(Dents, [H | _] = CStack, TokenAcc) when Dents > H ->
  erlang:error({strange_indentation,
      {line, get(line_num)},
      {curr_dents, Dents},
      {ind_stack, CStack},
      {token_acc, TokenAcc}});
do_dedent(Dents, [H | _] = CStack, TokenAcc) when Dents == H ->
  {CStack, TokenAcc};
do_dedent(Dents, [H | T], TokenAcc) when Dents < H ->
  do_dedent(Dents, T, [{dedent, get(line_num)} | TokenAcc]).


parse_inner(Line, InAttr) ->
  parse_inner(Line, none, [], [], InAttr).

%%
%% parse_inner(CurrString, LastToken, CurrentTokenAcc, AllTokenAcc)
%%
% Line is finished.  Flush token and return results.
parse_inner([], _, CurrTAcc, AllTAcc, InAttr) ->
  {?SFLUSH, InAttr};

% Escaped characters always get added to current token
% Adding "nothing" as last character ensures you can do
% "\|# this is not a comment #|" etc.
% Also note that it pops the esc code off CurrTAcc first.
parse_inner([Any | T], ?T_ESC, [?T_ESC | CurrTAcc], AllTAcc, InAttr) ->
  parse_inner(T, nothing, [Any | CurrTAcc], AllTAcc, InAttr);

% String started
parse_inner([?T_STR_MLT_ST_2 | T], ?T_STR_MLT_ST_1, [_ | CurrTAcc], AllTAcc, InAttr) ->
  {Str, Remaining} = pull_in_string(T, fun line_pull_in_str/1),
  parse_inner(Remaining, none, [], ?FLUSH({string, get(line_num), Str}), InAttr);

% Multi-line comment started
parse_inner([?T_IGN_MLT_ST_2 | T], ?T_IGN_MLT_ST_1, [_ | CurrTAcc], AllTAcc, InAttr) ->
  {_Comment, Remaining} = pull_in_string(T, fun line_pull_in_ign/1),
  parse_inner(Remaining, none, [], ?SFLUSH, InAttr);

% Inline comment started
parse_inner([?T_IGN_INL_2 | _T], ?T_IGN_INL_1, [_ | CurrTAcc], AllTAcc, InAttr) ->
  {?SFLUSH, InAttr};

% Flush current token and start w/ attributes
parse_inner([?T_ATTR_ST | T], _Last, CurrTAcc, AllTAcc, false) ->
  parse_inner(T, ?T_ATTR_ST, [],
    ?FLUSH({start_attrs, get(line_num)}), true);

% Flush current token and finish attributes
parse_inner([?T_ATTR_EN | T], _Last, CurrTAcc, AllTAcc, true) ->
  parse_inner(T, ?T_ATTR_EN, [], ?FLUSH({finish_attrs, get(line_num)}), false);

% Code chunk in main body
parse_inner([?T_CODE_ST | T], _, CurrTAcc, AllTAcc, false) ->
  {Code, Remaining} = pull_in_code(T),
  StartLNum = get(line_num),
  parse_inner(Remaining, none, [],
    ?FLUSH([{finish_code, get(line_num)} | [{string, get(line_num), Code} |
      [{start_code, StartLNum}]]]), false);

% Whitespace.  Flush token.
parse_inner([H | T], _Last, CurrTAcc, AllTAcc, InAttr)
when ?IS_WHITESPACE(H) ->
  parse_inner(T, H, [], ?SFLUSH, InAttr);

% Inline tag delimiter
parse_inner([?T_INL_TAG_D | T], _LAST, CurrTAcc, AllTAcc, false) ->
  parse_inner(T, ?T_INL_TAG_D, [],
    ?FLUSH({inline_delim, get(line_num)}), false);

parse_inner([?T_TAG_ST | T], _, [], AllTAcc, false) ->
  parse_inner(T, ?T_TAG_ST, [],
    [{start_tag, get(line_num), normal} | AllTAcc], false);

parse_inner([?T_TAG_SPECIAL_ST | T], _, [], AllTAcc, false) ->
  parse_inner(T, ?T_TAG_SPECIAL_ST, [],
    [{start_tag, get(line_num), special} | AllTAcc], false);

parse_inner([?T_TAG_CLASS_ST | T], _, [], AllTAcc, false) ->
  parse_inner(T, ?T_TAG_CLASS_ST, [],
    [{start_tag, get(line_num), class} | AllTAcc], false);

parse_inner([?T_TAG_ID_ST | T], _, [], AllTAcc, false) ->
  parse_inner(T, ?T_TAG_ID_ST, [],
    [{start_tag, get(line_num), id} | AllTAcc], false);

parse_inner([?T_ATTR_DELIM, H | T], _, CurrTAcc, AllTAcc, true)
when ?IS_WHITESPACE(H) orelse H == ?T_ATTR_EN ->
  parse_inner(T, ?T_ATTR_DELIM, [],
    ?FLUSH({attr_delim, get(line_num)}), true);

parse_inner([H | T], _Last, CurrTAcc, AllTAcc, InAttr) ->
  parse_inner(T, H, [H | CurrTAcc], AllTAcc, InAttr).


pull_in_string(Line, InnerFun) ->
  pull_in_string(Line, [], get(line_num), InnerFun).
pull_in_string(Line, Acc, LN, InnerFun) ->
  {StrAcc, Finished, Tail} = InnerFun(Line),
  case Finished of
    true ->
      put(line_num, LN),
      {lists:flatten(lists:reverse([StrAcc | Acc])), Tail};
    false ->
      Next = get(next_fun),
      case Next() of
        eof ->
          erlang:error({string_or_comment_not_closed_before_eof,
              {line, get(line_num)}});
        {error, Reason} ->
          erlang:error({input_zml_file_read_error, Reason});
        NewLine ->
          pull_in_string(NewLine, [StrAcc | Acc], LN + 1, InnerFun)
      end
  end.

% Mostly copies pull_in_string but had to make a new one (sigh) because I
% needed to pass state for the brackets.
pull_in_code(Line) ->
  pull_in_code(Line, [], get(line_num), 0).
pull_in_code(Line, Acc, LN, BrLvl) ->
  {StrAcc, Finished, Tail, BrLvl2} = line_pull_in_code(Line, none, BrLvl, []),
  case Finished of
    true ->
      put(line_num, LN),
      {lists:flatten(lists:reverse([StrAcc | Acc])), Tail};
    false ->
      Next = get(next_fun),
      case Next() of
        eof ->
          erlang:error({code_block_not_closed_before_eof,
            {line, get(line_num)}});
        {error, Reason} ->
          erlang:error({input_zml_file_read_error, Reason});
        NewLine ->
          pull_in_code(NewLine, [StrAcc | Acc], LN + 1, BrLvl2)
      end
  end.
line_pull_in_str(Line) ->
  line_pull_in_str(Line, none, []).
line_pull_in_str([Any | T], ?T_ESC, [?T_ESC | Acc]) ->
  line_pull_in_str(T, nothing, [Any | Acc]);
line_pull_in_str([?T_STR_MLT_EN_2 | T], ?T_STR_MLT_EN_1, [_ | Acc]) ->
  {lists:reverse(Acc), true, T};
line_pull_in_str([], _, Acc) ->
  {lists:reverse(Acc), false, []};
line_pull_in_str([H | T], _, Acc) ->
  line_pull_in_str(T, H, [H | Acc]).

line_pull_in_code([Any | T], ?T_ESC, BLvl, [?T_ESC | Acc]) ->
  line_pull_in_code(T, none, BLvl, [Any | Acc]);
line_pull_in_code([?T_CODE_ST | T], _, BLvl, Acc) ->
  line_pull_in_code(T, none, BLvl+1, [?T_CODE_ST | Acc]);
line_pull_in_code([?T_CODE_EN | T], _, 0, Acc) ->
  {lists:reverse(Acc), true, T, 0};
line_pull_in_code([?T_CODE_EN | T], _, BLvl, Acc) ->
  line_pull_in_code(T, none, BLvl - 1, [?T_CODE_EN | Acc]);
line_pull_in_code([], _, BLvl, Acc) ->
  {lists:reverse(Acc), false, [], BLvl};
line_pull_in_code([H | T], _, BLvl, Acc) ->
  line_pull_in_code(T, H, BLvl, [H | Acc]).

% Should combine this with the above via macro probably.  Or something.  As far
% as I can think at the moment I need the tokens compiled into the function
% matchers, hence the two sets of almost the same thing.
line_pull_in_ign(Line) ->
  line_pull_in_ign(Line, none, []).
line_pull_in_ign([Any | T], ?T_ESC, [?T_ESC | Acc]) ->
  line_pull_in_ign(T, nothing, [Any | Acc]);
line_pull_in_ign([?T_IGN_MLT_EN_2 | T], ?T_IGN_MLT_EN_1, [_ | Acc]) ->
  {lists:reverse(Acc), true, T};
line_pull_in_ign([], _, Acc) ->
  {lists:reverse(Acc), false, []};
line_pull_in_ign([H | T], _, Acc) ->
  line_pull_in_ign(T, H, [H | Acc]).
