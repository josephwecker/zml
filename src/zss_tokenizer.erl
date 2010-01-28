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
  case Next() of
    eof ->
      io:format("Done!", []);
    {error, Reason} ->
      erlang:error({input_zss_read_error, Reason});
    Line ->
      io:format("---> ~s", [Line]),
      tokenize_lines(Next, IndStack, RTokens, State)
  end.





