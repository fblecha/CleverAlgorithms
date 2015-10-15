%% @author Frank Blecha
%% @since 1/10/2008
%% @version 0.1
-module(barn).
-export([perms/1,random_select/2]).
-import(random).
-import(lists).


perms([]) -> [[]];
perms(L) -> [ [H|T] || H <- L,T <- perms(L--[H])].
    

random_select(L,N) ->
    random_select(N,L,[]).
random_select(N,In,Out) ->
    I = random:uniform(length(In)),
    Out1 = [ lists:nth(I,In) | Out],
    In1 = remove_at(In,I),
    if 
	length(Out1) == N ->
	    Out1;
	true  ->
	    random_select(N,In1,Out1)
    end.



remove_at(L,N) ->
    Elem = lists:nth(N,L),
    lists:delete(Elem,L).
