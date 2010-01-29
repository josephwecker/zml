-module(zss_tests).

-include_lib("eunit/include/eunit.hrl").

-import(zss, [cstr/1]).

%% Basic
basic_test_() -> [
  % Children
  ?_assertEqual([{["a b cde"], [{"font-weight", "bold"}]}],
cstr("a b cde
  :font-weight bold")),

  % Multiple selectors
  ?_assertEqual([{["a","b cde"], [{"font-weight", "bold"}]}],
cstr("a, b cde
  :font-weight bold")),

  % Multiple attributes
  ?_assertEqual([{["a", "b cde"], [{"color", "#333"}, {"float", "left"}]}],
cstr("a, b cde
  :color #333
  :float left")),

  % Nested selectors
  ?_assertEqual([{["a b"], [{"color", "#333"}]}],
cstr("a
  b
    :color #333")),

  % Double nested selectors
  ?_assertEqual([{["a b"], [{"color","#333"}]}, {["a cde"], [{"color", "#fff"}]}],
cstr("a
  b
    :color #333
  cde
    :color #fff")),

  % Multiplied nested selectors
  ?_assertEqual([
    {["a c", "a d", "b c", "b d"], [{"color", "#111"}]},
    {["a e", "a f", "b e", "b f"], [{"color", "#222"}]}],
cstr("a, b
  c, d
    :color #111
  e, f
    :color #222")),

  % Class and ID
  ?_assertEqual([
    {["#c .small", "#d.e .small", ".a .b .small", ".e#fgh .small"],
      [{"font-size", ".1em"}]}],
cstr("
.a .b, #c, #d.e, .e#fgh
  .small
    :font-size .1em")),

  % Attached parent
  ?_assertEqual([
    {[todo]}],
cstr("
.a b c, d
  &.wow
    :color 0
  &:hover, &:active
    :color #111
")),
  ?_assertEqual([
    {[todo]}],
cstr("div
  &.span-
    &1, &2
      :color 0"))

  ].

  
  % Comments
  % Multi-line comments
  % Ignored whitespaces
  % Parents over multiple lines
  % Nested parents over multiple lines
  % Escape codes (inc. for would-be variables)
  % Combined attribute families
  % Attribute values + families ( :border blah blah\n  :top blah blah\n  :bottom blah blah )
  % Special characters like in input
  % Colons before attributes ( *:first-child:before :content "blah"
  % Inline attribute
  % Children with inline attributes
  % Multiple inline attributes
  % Multiple parents with an inline attribute
  % Inline attributes plus children attributes
  % Inline plush children plus children selectors

  % Variable assign & echo
  % Variable echo alternate syntax
  % Variable reassign
  % Expression evaluation
  % Expressions with static ( :margin (!gi*6) 0 1.5em (-!gi+3)
  % Special variable type expressions
  % Special variable type warnings / errors on non-compatible
  % Type coersion

  % Includes
