%% @author Frank Blecha
%% @since 1/10/2008
%% @version 0.1

-module(stat).
-export([avg/2]).


-import(lists).

%% Fun should return a numeric to be used to compute the avg, given an
%% element of L.  Fun(Elem) should return a numeric
avg(L,Fun) ->
    Add = fun(Elem, AccIn) -> Fun(Elem) + AccIn end,
    Sum = lists:foldl(Add,0,L),
    Avg = Sum / length(L),
    Avg.
    
