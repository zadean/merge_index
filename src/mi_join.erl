-module(mi_join).

%% ====================================================================
%% API functions
%% ====================================================================
-export([merge/1]).

-define(RESULT_SIZE, 500).
-define(EOF, fun() -> eof end).


%% Sorted-merge join of multiple iterators
merge(Iters) when is_list(Iters) ->
   fun() -> merge_(Iters) end.

merge_([Iter1,Iter2]) -> 
   lm(Iter1,Iter2);
merge_([Iter1,Iter2|Rest]) ->
   merge_([lm(Iter1,Iter2)|Rest]).

% list-merge two iterators
lm(I1,I2) when is_function(I1), is_function(I2) -> lm(I1(),I2(), []);
lm(I1,I2) when is_function(I1) -> lm(I1(),I2, []);
lm(I1,I2) when is_function(I2) -> lm(I1,I2(), []);
lm(I1,I2) -> lm(I1,I2, []).

lm(eof,eof,Acc) ->
   {lists:reverse(Acc), ?EOF};
lm(eof,Rest,Acc) ->
   ok = consume(Rest),
   {lists:reverse(Acc), ?EOF};
lm(Rest,eof,Acc) ->
   ok = consume(Rest),
   {lists:reverse(Acc), ?EOF};
lm(I1,I2,Acc) when length(Acc) > ?RESULT_SIZE ->
   {lists:reverse(Acc), fun() -> lm(I1,I2,[]) end};
lm({V1,F1},{V2,F2},Acc0) ->
   case inter(V1,V2,[]) of
      {[],[],[]} ->
         lm(F1(),F2(),Acc0);
      {[],[],Acc} ->
         lm(F1(),F2(), Acc ++ Acc0);
      {[],R2,[]} ->
         lm(F1(),{R2,F2},Acc0);
      {[],R2,Acc} ->
         lm(F1(),{R2,F2},Acc ++ Acc0);
      {R1,[],[]} ->
         lm({R1,F1},F2(),Acc0);
      {R1,[],Acc} ->
         lm({R1,F1},F2(),Acc ++ Acc0)
   end.

% intersection of two usorted lists
% returns {LeftRest, RightRest, ReversedAcc}
inter([],L2,Acc) ->
   {[],L2,Acc};
inter(L1,[],Acc) ->
   {L1,[],Acc};
inter([H1|T1],[H2|T2],Acc) when H1 < H2 ->
   inter(T1,[H2|T2],Acc);
inter([H1|T1],[H2|T2],Acc) when H1 > H2 ->
   inter([H1|T1],T2,Acc);
inter([H1|T1],[_H2|T2],Acc) ->
   inter(T1,T2,[H1|Acc]).

% flush any left-overs
consume(eof) -> ok;
consume({_,Iter}) ->
   consume(Iter()).


