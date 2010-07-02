%% Special handler for *with code tags in zml
%%
%% Author: Sergei Matusevich <sergei.matusevich@gmail.com>
%%

-module(zml_special_with).

-export([process_node/2]).

process_node({{"with", _ID}, special, Attr, Children}, _Options) ->
  {with, proplists:get_value("id", Attr, []), Children}.

