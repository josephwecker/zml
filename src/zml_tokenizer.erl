
%%
%% For each line figure out tokens:
%% INDENTS / DEDENTS | TAG | (ATT VAL+)

-module(zml_tokenizer).


-compile(export_all).


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
					parse_lines(File, NewStack, [MoreTokens | RTokens])
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
	case get(in_attributes) of
		true ->
			{CStack, parse_inner(Line)};
		false ->
			do_parse_line(Dents, CStack, Line)
	end.
do_parse_line(Dents, [NLast | _] = CStack, Line) when Dents == NLast ->
	{CStack, parse_inner(Line)};
do_parse_line(Dents, [NLast | _] = CStack, Line) when Dents > NLast ->
	{[Dents | CStack], [parse_inner(Line), {indent, get(line_num)}]};
do_parse_line(Dents, CStack, Line) ->
	{NewStack, DentTokens} = do_dedent(Dents, CStack, []),
	{NewStack, [parse_inner(Line), DentTokens]}.

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
	parse_inner(Line, false, [], []).

%%
%% parse_inner(CurrString, IsEscaped, CurrentTokenAcc, AllTokenAcc)
%%

% String is finished.  Flush token and return results.
parse_inner([], _, CurrTAcc, AllTAcc) ->
	case CurrTAcc of
		[] ->
			AllTAcc;
		_ ->
			[{{word, lists:reverse(CurrTAcc)}, get(line_num)} | AllTAcc]
	end;

% Begin escaping a character
parse_inner([$\\ | T], false, CurrTAcc, AllTAcc) ->
	parse_inner(T, true, CurrTAcc, AllTAcc);

% Escaped characters always get added to current token
parse_inner([Any | T], true, CurrTAcc, AllTAcc) ->
	parse_inner(T, false, [Any | CurrTAcc], AllTAcc);

% Flush current token and start w/ attributes
parse_inner([$( | T], false, CurrTAcc, AllTAcc) ->
	put(in_attributes, true),
	case CurrTAcc of
		[] ->
			parse_inner(T, false, [],
				[{start_attrs, get(line_num)} | AllTAcc]);
		_ ->
			parse_inner(T, false, [],
				[[{start_attrs, get(line_num)},
				  {{word, lists:reverse(CurrTAcc)}, get(line_num)}] | AllTAcc])
	end;
% Flush current token and finish attributes
parse_inner([$) | T], false, CurrTAcc, AllTAcc) ->
	put(in_attributes, false),
	case CurrTAcc of
		[] ->
			parse_inner(T, false, [],
				[{finish_attrs, get(line_num)} | AllTAcc]);
		_ ->
			parse_inner(T, false, [],
				[[{finish_attrs, get(line_num)},
				  {{word, lists:reverse(CurrTAcc)}, get(line_num)}] | AllTAcc])
	end;

% Whitespace.  Flush token.
parse_inner([H | T], false, CurrTAcc, AllTAcc) when
		((H >= $\x{0009}) and (H =< $\x{000D}))
		or (H == $\x{0020}) or (H == $\x{00A0}) ->
	case CurrTAcc of
		[] ->
			parse_inner(T, false, [], AllTAcc);
		_ ->
			parse_inner(T, false, [],
				[{{word, lists:reverse(CurrTAcc)}, get(line_num)} | AllTAcc])
	end;

parse_inner([H | T], false, CurrTAcc, AllTAcc) ->
	parse_inner(T, false, [H | CurrTAcc], AllTAcc).



%% TODO:
%%   - Comments
%%   - Strings
%%   - Template coding
%%   - If possible- cognizant of whether () are really for attributes
%%
