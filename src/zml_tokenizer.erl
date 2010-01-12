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
%%   - Probably need to do something with tags.  We'll see when I do the
%%     parser.
%%
%% EVENTUALLY:
%%   - Factor out all the ugly "case CurrTAcc of" stuff all over
%%   - Use macros or something to improve readability of the main tokenizing
%%     loops.
%%
%% NOTES:
%%   - I might just let the parser decide when to ignore false
%%     "start/end_attrs" tokens
%%   - Might need to generate a token for ";" for inline tags
%%   - (Make sure inline is also terminated by EOL)
%%   - Template encoding ("[...]") will be handled post-parser probably
%%

-module(zml_tokenizer).

-compile(export_all).


-define(T_ATTR_ST, $().
-define(T_ATTR_EN, $)).
-define(T_ESC, $\\).

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

-define(T_INL_TAG_D, $;).


parse_file(Filename) when is_list(Filename) ->
	{ok, File} = file:open(Filename, [read]),
	erase(),
	put(line_num, 0),
	put(in_attributes, false),
	parse_lines(File, [0], []).  % Just ignore leading "indent" token if needed

parse_lines(File, IndentStack, RTokens) ->
	put(file, File),
	put(line_num, get(line_num) + 1),
	case io:get_line(File, "") of
		eof ->
			lists:reverse([{end_of_file, get(line_num) - 1} |
					lists:flatten(RTokens)]);
		{error, Reason} ->
			erlang:error({input_zml_file_read_error, Reason});
		Line ->
			{NumDents, RemainingLine} = get_dent(Line),
			case RemainingLine of
				[] -> % Blank line
					parse_lines(File, IndentStack, RTokens);
				_ ->
					{NewStack, MoreTokens} =
						parse_line(NumDents, IndentStack, RemainingLine),
					case MoreTokens of
						[] ->
							parse_lines(File, NewStack, RTokens);
						_ ->
							parse_lines(File, NewStack, [MoreTokens | RTokens])
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

%% Figures out indent / dedent tokens and then calls parse_inner to figure out
%% the rest of whatever is there.  Skips indent/outdent if currently processing
%% attributes.
parse_line(Dents, CStack, Line) ->
	StartingInAttributes = get(in_attributes),
	ParsedLine = parse_inner(Line),
	case ParsedLine of
		[] ->
			{CStack, []};
		_ ->
			case StartingInAttributes of
				true ->
					{CStack, ParsedLine};
				false ->
					do_parse_line(Dents, CStack, ParsedLine)
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


parse_inner(Line) ->
	parse_inner(Line, none, [], []).

%%
%% parse_inner(CurrString, LastToken, CurrentTokenAcc, AllTokenAcc)
%%
% Line is finished.  Flush token and return results.
parse_inner([], _, CurrTAcc, AllTAcc) ->
	case CurrTAcc of
		[] ->
			AllTAcc;
		_ ->
			[{word, get(line_num), lists:reverse(CurrTAcc)} | AllTAcc]
	end;

% Escaped characters always get added to current token
% Adding "nothing" as last character ensures you can do
% "\|# this is not a comment #|" etc.
% Also note that it pops the esc code off CurrTAcc first.
parse_inner([Any | T], ?T_ESC, [?T_ESC | CurrTAcc], AllTAcc) ->
	parse_inner(T, nothing, [Any | CurrTAcc], AllTAcc);

% String started
parse_inner([?T_STR_MLT_ST_2 | T], ?T_STR_MLT_ST_1, [_ | CurrTAcc], AllTAcc) ->
	{Str, Remaining} = pull_in_string(T, fun line_pull_in_str/1),
	case CurrTAcc of
		[] ->
			parse_inner(Remaining, none, [],
				[{word, get(line_num), Str} | AllTAcc]);
		_ ->
			parse_inner(Remaining, none, [],
				[[{word, get(line_num), Str},
				  {word, get(line_num), lists:reverse(CurrTAcc)}] | AllTAcc])
	end;


% Multi-line comment started
parse_inner([?T_IGN_MLT_ST_2 | T], ?T_IGN_MLT_ST_1, [_ | CurrTAcc], AllTAcc) ->
	{_Comment, Remaining} = pull_in_string(T, fun line_pull_in_ign/1),
	case CurrTAcc of
		[] ->
			parse_inner(Remaining, none, [], AllTAcc);
		_ ->
			parse_inner(Remaining, none, [],
				[{word, get(line_num), lists:reverse(CurrTAcc)} | AllTAcc])
	end;

% Inline comment started
parse_inner([?T_IGN_INL_2 | _T], ?T_IGN_INL_1, CurrTAcc, AllTAcc) ->
	case CurrTAcc of
		[?T_IGN_INL_1] ->
			AllTAcc;
		[?T_IGN_INL_1 | Earlier]  ->
			[{word, get(line_num), lists:reverse(Earlier)} | AllTAcc]
	end;

% Flush current token and start w/ attributes
parse_inner([?T_ATTR_ST | T], _Last, CurrTAcc, AllTAcc) ->
	put(in_attributes, true),
	case CurrTAcc of
		[] ->
			parse_inner(T, ?T_ATTR_ST, [],
				[{start_attrs, get(line_num)} | AllTAcc]);
		_ ->
			parse_inner(T, ?T_ATTR_ST, [],
				[[{start_attrs, get(line_num)},
				  {word, get(line_num), lists:reverse(CurrTAcc)}] | AllTAcc])
	end;
% Flush current token and finish attributes
parse_inner([?T_ATTR_EN | T], _Last, CurrTAcc, AllTAcc) ->
	put(in_attributes, false),
	case CurrTAcc of
		[] ->
			parse_inner(T, ?T_ATTR_EN, [],
				[{finish_attrs, get(line_num)} | AllTAcc]);
		_ ->
			parse_inner(T, ?T_ATTR_EN, [],
				[[{finish_attrs, get(line_num)},
				  {word, get(line_num), lists:reverse(CurrTAcc)}] | AllTAcc])
	end;

% Whitespace.  Flush token.
parse_inner([H | T], _Last, CurrTAcc, AllTAcc) when
		((H >= $\x{0009}) and (H =< $\x{000D}))
		or (H == $\x{0020}) or (H == $\x{00A0}) ->
	case CurrTAcc of
		[] ->
			parse_inner(T, H, [], AllTAcc);
		_ ->
			parse_inner(T, H, [],
				[{word, get(line_num), lists:reverse(CurrTAcc)} | AllTAcc])
	end;

% Inline tag delimiter
parse_inner([?T_INL_TAG_D | T], _LAST, CurrTAcc, AllTAcc) ->
	case CurrTAcc of
		[] ->
			parse_inner(T, ?T_INL_TAG_D, [], [{inline_delim, get(line_num)} | AllTAcc]);
		_ ->
			parse_inner(T, ?T_INL_TAG_D, [],
				[[{inline_delim, get(line_num)},
				  {word, get(line_num), lists:reverse(CurrTAcc)}] | AllTAcc])
	end;

parse_inner([H | T], _Last, CurrTAcc, AllTAcc) ->
	parse_inner(T, H, [H | CurrTAcc], AllTAcc).


pull_in_string(Line, InnerFun) ->
	pull_in_string(Line, [], get(line_num), InnerFun).
pull_in_string(Line, Acc, LN, InnerFun) ->
	{StrAcc, Finished, Tail} = InnerFun(Line),
	case Finished of
		true ->
			put(line_num, LN),
			{lists:flatten(lists:reverse([StrAcc | Acc])), Tail};
		false ->
			File = get(file),
			case io:get_line(File, "") of
				eof ->
					erlang:error({string_or_comment_not_closed_before_eof,
							{line, get(line_num)}});
				{error, Reason} ->
					erlang:error({input_zml_file_read_error, Reason});
				NewLine ->
					pull_in_string(NewLine, [StrAcc | Acc], LN + 1, InnerFun)
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
