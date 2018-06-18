-module(mi_join).

%% ====================================================================
%% API functions
%% ====================================================================
-export([merge/1,
         union/1]).

-define(UNION_SIZE, 200).
-define(RESULT_SIZE, 500).
-define(EOF, fun() -> eof end).


%% Sorted-merge join of multiple iterators
merge(Iters) when is_list(Iters) ->
   fun() -> merge_(Iters) end.

%% Sorted-union join of multiple iterators
union(Iters) when is_list(Iters) ->
   fun() -> union_(Iters) end.

merge_([Iter1,Iter2]) -> 
   lm(Iter1,Iter2);
merge_([Iter1,Iter2|Rest]) ->
   merge_([lm(Iter1,Iter2)|Rest]).

union_([Iter1,Iter2]) -> 
   lu(Iter1,Iter2);
union_([Iter1,Iter2|Rest]) ->
   union_([lu(Iter1,Iter2)|Rest]).

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


% list-union two iterators
lu(I1,I2) when is_function(I1), is_function(I2) -> lu(I1(),I2(), []);
lu(I1,I2) when is_function(I1) -> lu(I1(),I2, []);
lu(I1,I2) when is_function(I2) -> lu(I1,I2(), []);
lu(I1,I2) -> lu(I1,I2, []).

lu(eof,eof,Acc) ->
   {lists:reverse(Acc), ?EOF};
lu(eof,Rest,Acc) ->
   {lists:reverse(Acc), fun() -> Rest end};
lu(Rest,eof,Acc) ->
   {lists:reverse(Acc), fun() -> Rest end};
lu(I1,I2,Acc) when length(Acc) > ?UNION_SIZE ->
   {lists:reverse(Acc), fun() -> lu(I1,I2,[]) end};
lu({V1,F1},{V2,F2},Acc0) ->
   case union(V1,V2,[]) of
      {[],[],[]} ->
         lu(F1(),F2(),Acc0);
      {[],[],Acc} ->
         lu(F1(),F2(), Acc ++ Acc0);
      {[],R2,[]} ->
         lu(F1(),{R2,F2},Acc0);
      {[],R2,Acc} ->
         lu(F1(),{R2,F2},Acc ++ Acc0);
      {R1,[],[]} ->
         lu({R1,F1},F2(),Acc0);
      {R1,[],Acc} ->
         lu({R1,F1},F2(),Acc ++ Acc0)
   end.



% union of two usorted lists
% idea here is to work up to at least RESULT_SIZE values that are safe,
% big lists == HUGE unions
% iterator function will contain any unsafe left-overs to use in the next call
% as the base list
% returns {UnsafeRestLeft, UnsafeRestRight, ReveredUnionAcc}
union([],R2,Acc) ->
   {[],R2,Acc};
union(R1,[],Acc) ->
   {R1,[],Acc};
union([H1|T1],[H2|_] = S2,Acc) when H1 < H2 ->
   union(T1,S2,[H1|Acc]);
union([H1|_] = S1,[H2|T2],Acc) when H1 > H2 ->
   union(S1,T2,[H2|Acc]);
union([H1|T1],[_|T2],Acc) -> %when H1 == H2 ->
   union(T1,T2,[H1|Acc]).



   


