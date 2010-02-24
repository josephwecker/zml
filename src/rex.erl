-module(rex).
-export([has_match/2, first_match/2, matches/2, matches/3, sub_matches/2,
		replacement_pipe_i/2]).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_RE_OPTIONS, [{capture, all, binary}, global]).


replacement_pipe_i(Start, RPipeline) ->
	lists:foldl(fun rpipe_inner/2, Start, RPipeline).

rpipe_inner({RE, Replacement}, Subject) ->
	{ok, MP} = re:compile(RE, [caseless, dotall]),
	re:replace(Subject, MP, Replacement, [global]).

has_match(Haystack, Regex) ->
	case matches(Haystack, Regex, [{capture, none}]) of
		true -> true;
		[] -> false
	end.

first_match(Haystack, Regex) ->
	[Match] = matches(Haystack, Regex, [{capture, first, binary}]),
	Match.

matches(Haystack, Regex) -> 
	matches(Haystack, Regex, ?DEFAULT_RE_OPTIONS).  % TODO - combine these in below
matches(Haystack, Regex, Options) when is_atom(Regex) ->
	matches(Haystack, atom_to_list(Regex), Options);
matches(Haystack, Regex, Options) -> 
	% TODO - Cache compiled regex's?
	{ok, CompiledRegex} = re:compile(Regex, [unicode, dotall]),
	case re:run(Haystack, CompiledRegex, Options) of
		match -> true;
		nomatch -> [];
		{match, Matches} -> Matches
	end.

sub_matches(Haystack,Regex) -> 
	matches(Haystack, Regex, [{capture, all_but_first, binary}, global]).

has_match_test_() -> [
		?_assert(has_match(<<"hey there.">>, <<"there">>)),
		?_assert(has_match(<<"hey there.">>, "there")),
		?_assert(has_match(<<"hey there.">>, there)),
		?_assert(has_match("hey there.", "there")),
		?_assert(not has_match(<<"hey there.">>, <<"oink">>)),
		?_assert(not has_match(<<"hey there.">>, "oink")),
		?_assert(not has_match(<<"hey there.">>, oink))
	].

first_match_test_() -> [
		?_assertEqual(<<"dink">>, first_match("dinkledink", dink)),
		?_assertEqual(<<"ko">>, first_match(<<"doinkodoinkokokoko">>, "ko")),
		?_assertEqual(<<"e b">>, first_match("danke blah", ". b")),
		?_assertEqual(<<"ederma">>, first_match("feedermandle", "e(der(ma))"))
	].

matches_test_() -> [
		?_assertEqual([[<<"ei">>],[<<"ei">>]], matches("eieio", "ei")),
		?_assertEqual([[<<"ei">>,<<"i">>], [<<"ei">>,<<"i">>]], matches("eieio", "e(i)"))
	].

sub_matches_test_() -> [
		?_assertEqual([[<<"i">>],[<<"i">>]], sub_matches("eieio", "e(i)")),
		?_assertEqual([[<<"tha">>]], sub_matches("barthalemue", "ar(tha)lem"))
	].
