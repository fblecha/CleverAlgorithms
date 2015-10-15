-module(bin_util).
-export([bit_count/1]).

bit_count(N) ->
    bit_count_acc(N,0).


bit_count_acc(0, Acc) -> Acc;
bit_count_acc(N, Acc) ->
    NewAcc = Acc + (N band 1),
    NewN = N bsr 1,
    bit_count_acc(NewN,NewAcc).

    
