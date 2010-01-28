-module(zss_tokenizer).

-export([tokenize_str/1, tokenize_file/1, tokenize_stream/1]).

tokenize_str(InStr) ->
  erase(),
  put(line_num, 0),
  put('--input-string--', InStr),
  tokenize_lines(fun string_feed/0, [0], [], {}).

tokenize_file(FName) ->
  {ok, File} = file:open(FName, [read]),
  tokenize_stream(File).

tokenize_stream(Stream) ->
  erase(),
  put(line_num, 0),
  tokenize_lines(fun() -> io:get_line(Stream, "") end, [0], [], {}).

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
  put(line_num, get(line_num) + 1),
  put(next_fun, Next),
  case Next() of
    eof ->
      LN = get(line_num) - 1,
      Tokens2 = lists:reverse(lists:flatten(RTokens)),
      ExtraDedents = lists:map(fun(_) -> {dedent, LN} end,
        lists:seq(1,length(IndStack) - 1)),
      Tokens2 ++ ExtraDedents ++ [{end_of_file, LN}];
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


% Process the line as if no indents/dedents, then only adds the newline,
% indents, dedents, etc. if there are tokens from the line (instead of, say
% just comments).
process_line(Dents, IStack, Line, State) ->
  {LineTokens, State2} = line_tokens(Line, State),
  case LineTokens of
    [] ->
      {IStack, [], State2};
    _ ->
      LineTokens2 = [{newline, get(line_num)} | LineTokens],
      {IStack2, LineTokens3} = process_dents(Dents, IStack, LineTokens2),
      {IStack2, LineTokens3, State2}
  end.


process_dents(Dents, [NLast | _] = IStack, Toks) when Dents == NLast ->
  {IStack, Toks};
process_dents(Dents, [NLast | _] = IStack, Toks) when Dents > NLast ->
  {[Dents | IStack], [Toks, {indent, get(line_num)}]};
process_dents(Dents, IStack, Toks) ->
  {NewStack, DentTokens} = do_dedent(Dents, IStack, []),
  %{NewStack, [Toks, DentTokens]}.
  {NewStack, Toks ++ DentTokens}.

% Pops indents off the stack until it matches the current indent- issuing a
% "dedent" for each one.
% Throws an error if the current indent is not aligned with some past
% indentation level.
do_dedent(_, [], TokenAcc) ->
  {[], TokenAcc};
do_dedent(Dents, [H | _] = IStack, TokenAcc) when Dents > H ->
  erlang:error({strange_indentation,
      {line, get(line_num)},
      {curr_dents, Dents},
      {ind_stack, IStack},
      {token_acc, TokenAcc}});
do_dedent(Dents, [H | _] = IStack, TokenAcc) when Dents == H ->
  {IStack, TokenAcc};
do_dedent(Dents, [H | T], TokenAcc) when Dents < H ->
  do_dedent(Dents, T, [{dedent, get(line_num)} | TokenAcc]).


line_tokens(Line, State) ->
  {[{general, Line}], State}.
