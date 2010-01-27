-module(zss_tests).

-include_lib("eunit/include/eunit.hrl").

-import(zss, [cstr]).

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
    {[".a .b .small", "#c .small", "#d.e .small", ".e#fgh .small"],
     [{"font-size", ".1em"}]],
cstr("
.a .b, #c, #d.e, .e#fgh
  .small
    :font-size .1em")),

  % Attached parent
