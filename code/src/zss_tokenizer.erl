-module(zss_tokenizer).

-export([tokenize_string/1, tokenize_file/1, tokenize_stream/1]).

-define(FLUSH(Other),
  case CurrTAcc of
    [] -> [Other | AllTAcc];
    _ ->
      case State of
        {true} ->
          [[{attr, ?LN, Other}, {attr, ?LN, string:strip(lists:reverse(CurrTAcc))}] |
            AllTAcc];
        {false} ->
          [[{sel, ?LN, Other}, {sel, ?LN, string:strip(lists:reverse(CurrTAcc))}] |
            AllTAcc]
      end
  end).
-define(SFLUSH,
  case CurrTAcc of
    [] -> AllTAcc;
    _ ->
      case State of
        {true} ->
          [{attr, ?LN, string:strip(lists:reverse(CurrTAcc))} | AllTAcc];
        {false} ->
          Str = string:strip(lists:reverse(CurrTAcc)),
          case Str of
            [$[ | _] ->
              case string:chr(Str,$=) of
                0 ->
                  case Str of
                    "[include " ++ R ->
                      [{include, ?LN, string:strip(R)} | AllTAcc];
                    _ ->
                      [{code, ?LN, Str} | AllTAcc]
                  end;
                _ ->
                [{assignment, ?LN, Str} | AllTAcc]
              end;
            _ ->
              [{sel, ?LN, Str} | AllTAcc]
          end
      end
  end).
-define(LN, get(line_num)).

-define(T_IGN_INL_1, $|).
-define(T_IGN_INL_2, $|).

-define(T_IGN_MLT_ST_1, $|).
-define(T_IGN_MLT_ST_2, $#).
-define(T_IGN_MLT_EN_1, $#).
-define(T_IGN_MLT_EN_2, $|).

-define(T_STR_MLT_ST_1, $|).
-define(T_STR_MLT_ST_2, 34). % dbl-quote
-define(T_STR_MLT_EN_1, 34).
-define(T_STR_MLT_EN_2, $|).

-define(T_ESC, $\\).

-define(T_ATTR_ST, $:).
-define(T_PAR_SEP, $,).

-define(T_CODE_ST, $[).
-define(T_CODE_EN, $]).

tokenize_string(InStr) ->
  erase(),
  put(line_num, 0),
  put('--input-string--', InStr),
  tokenize_lines(fun string_feed/0, [0], [], {false}).

tokenize_file(FName) ->
  {ok, File} = file:open(FName, [read]),
  tokenize_stream(File).

tokenize_stream(Stream) ->
  erase(),
  put(line_num, 0),
  tokenize_lines(fun() -> io:get_line(Stream, "") end, [0], [], {false}).

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

tokenize_lines(Next, IndStack, RTokens, State) ->
  put(line_num, ?LN + 1),
  put(next_fun, Next),
  case Next() of
    eof ->
      LN = ?LN - 1,
      Tokens2 = lists:reverse(lists:flatten(RTokens)),
      ExtraDedents = lists:map(fun(_) -> {dedent, LN, none} end,
        lists:seq(1,length(IndStack) - 1)),
      Tokens2 ++ ExtraDedents ++ [{end_of_file, LN, none}];
    {error, Reason} ->
      erlang:error({input_zss_read_error, Reason});
    Line ->
      {NumDents, RemainingLine} = get_dent(Line),
      case RemainingLine of
        [] -> % Blank line
          tokenize_lines(Next, IndStack, RTokens, State);
        _ ->
          {NewStack, MoreTokens, State2} =
            process_line(NumDents, IndStack, RemainingLine, State),
          case MoreTokens of
            [] ->
              % Nothing to do- next line please
              tokenize_lines(Next, NewStack, RTokens, State2);
            _ ->
              tokenize_lines(Next, NewStack, [MoreTokens | RTokens], State2)
          end
      end
  end.

%% How many whitespace characters lead up the line.
%% (Calculated with unicode characters- that's why it looks funny)
get_dent(Line) ->
  get_dent(Line, 0).
get_dent([H|T],N) when
    ((H >= $\x{0009}) and (H =< $\x{000D}))
    or (H == $\x{0020}) or (H == $\x{00A0}) ->
  get_dent(T,N+1);
get_dent(Rest,N) ->
  {N, Rest}.


% Process the line as if no indents/dedents, then only adds the newline (n/a),
% indents, dedents, etc. if there are tokens from the line (instead of, say
% just comments).
process_line(Dents, IStack, Line, State) ->
  {LineTokens, State2} = line_tokens(Line, State),
  case LineTokens of
    [] ->
      {IStack, [], State2};
    _ ->
      {IStack2, LineTokens3} = process_dents(Dents, IStack, LineTokens),
      {IStack2, LineTokens3, State2}
  end.


process_dents(Dents, [NLast | _] = IStack, Toks) when Dents == NLast ->
  {IStack, Toks};
process_dents(Dents, [NLast | _] = IStack, Toks) when Dents > NLast ->
  {[Dents | IStack], [Toks, {indent, ?LN, none}]};
process_dents(Dents, IStack, Toks) ->
  {NewStack, DentTokens} = do_dedent(Dents, IStack, []),
  {NewStack, Toks ++ DentTokens}.

% Pops indents off the stack until it matches the current indent- issuing a
% "dedent" for each one.
% Throws an error if the current indent is not aligned with some past
% indentation level.
do_dedent(_, [], TokenAcc) ->
  {[], TokenAcc};
do_dedent(Dents, [H | _] = IStack, TokenAcc) when Dents > H ->
  erlang:error({strange_indentation,
      {line, ?LN},
      {curr_dents, Dents},
      {ind_stack, IStack},
      {token_acc, TokenAcc}});
do_dedent(Dents, [H | _] = IStack, TokenAcc) when Dents == H ->
  {IStack, TokenAcc};
do_dedent(Dents, [H | T], TokenAcc) when Dents < H ->
  do_dedent(Dents, T, [{dedent, ?LN, none} | TokenAcc]).


line_tokens(Line, _State) ->
  line_tokens(Line, none, [], [], {false}).
% Line is finished- move current-token to all, and return.
% Reset in-attribute state
line_tokens([], _, CurrTAcc, AllTAcc, State) ->
  {?SFLUSH, {false}};

% Escaped character.  Pops the escape char off current token, adds this
% character to the token no matter what it is, and sends 'none' as last token
% so that double-character tokens like |# can be escaped.
line_tokens([Any | T], ?T_ESC, [?T_ESC | CurrTAcc], AllTAcc, State) ->
  line_tokens(T, none, [Any | CurrTAcc], AllTAcc, State);

% String
line_tokens([?T_STR_MLT_ST_2 | T], ?T_STR_MLT_ST_1,
    [_ | CurrTAcc], AllTAcc, State) ->
  {Str, Remaining} = pull_inner(T, fun line_pull_in_str/1),
  line_tokens(Remaining, string, Str ++ CurrTAcc, AllTAcc, State);

% Inline comment - pretend we're at the end of the string
line_tokens([?T_IGN_INL_2 |_], ?T_IGN_INL_1, [_ | CurrTAcc], AllTAcc, State) ->
  {?SFLUSH, State};

% Multiline comment
line_tokens([?T_IGN_MLT_ST_2 | T], ?T_IGN_MLT_ST_1,
    [_ | CurrTAcc], AllTAcc, State) ->
  {_Comment, Remaining} = pull_inner(T, fun line_pull_in_ign/1),
  line_tokens(Remaining, none, [], ?SFLUSH, State);

% Starting an attribute
line_tokens([?T_ATTR_ST | T], _, [], AllTAcc, _State) ->
  line_tokens(T, ?T_ATTR_ST, [], AllTAcc, {true});
line_tokens([?T_ATTR_ST | T], $ , CurrTAcc, AllTAcc, State) ->
  line_tokens(T, ?T_ATTR_ST, [], ?SFLUSH, {true});

% Assignment or code
line_tokens([?T_CODE_ST | T], _, CurrTAcc, AllTAcc, {false} = _State) ->
  {Code, T2} = pull_inner(T, fun line_pull_in_code/1),
  line_tokens(T2, code, Code ++ [?T_CODE_ST] ++ CurrTAcc, AllTAcc, {false});

% Parent separator
line_tokens([?T_PAR_SEP | T], _, CurrTAcc, AllTAcc, {false} = State) ->
  line_tokens(T, ?T_PAR_SEP, [], ?SFLUSH, {false});

% Whitespace.  Flush token.
line_tokens([$\n | T], _, CurrTAcc, AllTAcc, State) ->
  line_tokens(T, $\n, [], ?SFLUSH, {false});

% Ignore consecutive spaces
line_tokens([$  | T], $ , CurrTAcc, AllTAcc, State) ->
  line_tokens(T, $ , CurrTAcc, AllTAcc, State);
line_tokens([$  | T], _, [], AllTAcc, State) ->
  line_tokens(T, $ , [], AllTAcc, State);

line_tokens([H | T], _Last, CurrTAcc, AllTAcc, State) ->
  line_tokens(T, H, [H | CurrTAcc], AllTAcc, State).


pull_inner(Line, InnerFun) ->
  pull_inner(Line, [], ?LN, InnerFun).
pull_inner(Line, Acc, LN, InnerFun) ->
  {StrAcc, Finished, Tail} = InnerFun(Line),
  case Finished of
    true ->
      put(line_num, LN),
      {lists:flatten([StrAcc | Acc]), Tail};
    false ->
      Next = get(next_fun),
      case Next() of
        eof ->
          erlang:error({string_or_comment_not_closed_before_eof,
              {line, ?LN}});
        {error, Reason} ->
          erlang:error({input_zss_read_error, Reason});
        NewLine ->
          pull_inner(NewLine, [StrAcc | Acc], LN + 1, InnerFun)
      end
  end.

line_pull_in_str(Line) ->
  line_pull_in_str(Line, none, []).
line_pull_in_str([Any | T], ?T_ESC, [?T_ESC | Acc]) ->
  line_pull_in_str(T, nothing, [Any | Acc]);
line_pull_in_str([?T_STR_MLT_EN_2 | T], ?T_STR_MLT_EN_1, [_ | Acc]) ->
  {Acc, true, T};
line_pull_in_str([], _, Acc) ->
  {Acc, false, []};
line_pull_in_str([H | T], _, Acc) ->
  line_pull_in_str(T, H, [H | Acc]).

line_pull_in_ign(Line) ->
  line_pull_in_ign(Line, none, []).
line_pull_in_ign([Any | T], ?T_ESC, [?T_ESC | Acc]) ->
  line_pull_in_ign(T, nothing, [Any | Acc]);
line_pull_in_ign([?T_IGN_MLT_EN_2 | T], ?T_IGN_MLT_EN_1, [_ | Acc]) ->
  {Acc, true, T};
line_pull_in_ign([], _, Acc) ->
  {Acc, false, []};
line_pull_in_ign([H | T], _, Acc) ->
  line_pull_in_ign(T, H, [H | Acc]).

line_pull_in_code(Line) ->
  line_pull_in_code(Line, 1, []).
line_pull_in_code([],_,Acc) ->
  {Acc, false, []};
line_pull_in_code([?T_CODE_ST | T], Lvl, Acc) ->
  line_pull_in_code(T, Lvl + 1, [?T_CODE_ST | Acc]);
line_pull_in_code([?T_CODE_EN | T], Lvl, Acc) ->
  Lvl2 = Lvl - 1,
  case Lvl2 of
    0 ->
      {[?T_CODE_EN | Acc], true, T};
    _ ->
      line_pull_in_code(T, Lvl2, [?T_CODE_EN | Acc])
  end;
line_pull_in_code([H | T], Lvl, Acc) ->
  line_pull_in_code(T, Lvl, [H | Acc]).
