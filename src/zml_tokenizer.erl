
%%
%% For each line figure out tokens:
%% INDENTS / DEDENTS | TAG | (ATT VAL+)

-module(zml_tokenizer).

-compile(export_all).


-define(T_ATTR_ST, $().
-define(T_ATTR_EN, $)).
-define(T_ESC, $\\).

-define(T_IGN_INL_1, $|).
-define(T_IGN_INL_2, $|).

-define(T_COMMENT_MULTI_ST_1, $|).
-define(T_COMMENT_MULTI_ST_2, $#).
-define(T_COMMENT_MULTI_EN_1, $#).
-define(T_COMMENT_MULTI_EN_2, $|).

-define(T_STRING_TYPE1, 34). % dbl-quote
-define(T_STRING_TYPE2, 39). % single-quote

-define(T_STRING_MULTI_ST_1, $|).
-define(T_STRING_MULTI_ST_2, 34).
-define(T_STRING_MULTI_EN_1, 34).
-define(T_STRING_MULTI_EN_2, $|).


parse_file(Filename) when is_list(Filename) ->
	{ok, File} = file:open(Filename, [read]),
	erase(),
	put(line_num, 0),
	put(in_attributes, false),
	parse_lines(File, [0], []).  % Just ignore leading "indent" token if needed

parse_lines(File, IndentStack, RTokens) ->
	put(line_num, get(line_num) + 1),
	case io:get_line(File, "") of
		eof ->
			lists:reverse(lists:flatten(RTokens));
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
%% parse_inner(CurrString, IsEscaped, CurrentTokenAcc, AllTokenAcc)
%%

% Line is finished.  Flush token and return results.
parse_inner([], _, CurrTAcc, AllTAcc) ->
	case CurrTAcc of
		[] ->
			AllTAcc;
		_ ->
			[{{word, lists:reverse(CurrTAcc)}, get(line_num)} | AllTAcc]
	end;

% Begin escaping a character
parse_inner([?T_ESC | T], _, CurrTAcc, AllTAcc) ->
	parse_inner(T, ?T_ESC, CurrTAcc, AllTAcc);

% Escaped characters always get added to current token
% Adding "nothing" as last character ensures you can do
% "\|# this is not a comment #|" etc.
parse_inner([Any | T], ?T_ESC, CurrTAcc, AllTAcc) ->
	parse_inner(T, nothing, [Any | CurrTAcc], AllTAcc);

% String started

% Multi-line comment started

% Inline comment started
parse_inner([?T_IGN_INL_2 | _T], ?T_IGN_INL_1, CurrTAcc, AllTAcc) ->
	case CurrTAcc of
		[?T_IGN_INL_1] ->
			AllTAcc;
		[?T_IGN_INL_1 | Earlier]  ->
			[{{word, lists:reverse(Earlier)}, get(line_num)} | AllTAcc]
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
				  {{word, lists:reverse(CurrTAcc)}, get(line_num)}] | AllTAcc])
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
				  {{word, lists:reverse(CurrTAcc)}, get(line_num)}] | AllTAcc])
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
				[{{word, lists:reverse(CurrTAcc)}, get(line_num)} | AllTAcc])
	end;

parse_inner([H | T], _Last, CurrTAcc, AllTAcc) ->
	parse_inner(T, H, [H | CurrTAcc], AllTAcc).



%% TODO:
%%   - Comments
%%   - Strings
%%   - Template coding
%%   - If possible- cognizant of whether () are really for attributes
%%
