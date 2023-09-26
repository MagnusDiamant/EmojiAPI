% proper lists; i.e. lists whose last tails are the empty list []
-module(createList).
-export([range/2]).

% Create list from x to y, e.g: lists:range(1,4) -> [1,2,3,4]
range(N, N) ->
    [N];
range(Min, Max) ->
    [Min | range(Min+1, Max)].
