%% -------------------------------------------------------------------
%%
%% mi_bloom: bloom filter adapted for use by merge index.
%%
%% Copyright (c) 2007-2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Implementation of the Bloom filter data structure.
%% @reference [http://en.wikipedia.org/wiki/Bloom_filter]

%% Adapted from http://code.google.com/p/bloomerl for use in
%% merge_index, where we are worried about speed of creating the bloom
%% filter and testing membership as well as size of the bloom
%% filter. By hard coding some parameters, we reduce the size. Also,
%% by calculating the bloom filter in batches, we improve the
%% performance.

-module(mi_bloom).
-export([new/1, is_element/2]).
-include("merge_index.hrl").

-ifdef(TEST).
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").
-endif.

%% These settings give us a max 256 keys with 0.05 error rate.
-define(M, 1600).
-define(K, 4).

%% @doc Generate a new bloom filter containing the specified keys.
new(Keys) ->
    OnBits = lists:usort(lists:flatten([calc_idxs(X) || X <- Keys])),
    list_to_bitstring(generate_bits(0, OnBits)).

generate_bits(Pos, [NextOnPos|T]) ->
    Gap = NextOnPos - Pos - 1,
    case Gap > 0 of
        true ->
            Bits = <<0:Gap/integer, 1:1/integer>>,
            [Bits|generate_bits(Pos + Gap + 1, T)];
        false ->
            Bits = <<1:1/integer>>,
            [Bits|generate_bits(Pos + 1, T)]
    end;
generate_bits(Pos, []) ->
    Gap = ?M - Pos,
    [<<0:Gap/integer>>].

%% @spec is_element(string(), bloom()) -> bool()
%% @doc Determines if the key is (probably) an element of the filter.
is_element(Key, Bitmap) -> 
    is_element(Key, Bitmap, calc_idxs(Key)).
is_element(Key, Bitmap, [Idx | T]) ->
    %% If we are looking for the first bit, do slightly different math
    %% than if we are looking for later bits.
   Bit1 =  
      case Idx > 0 of
           true ->
               PreSize = Idx - 1,
               <<_:PreSize/bits, Bit:1/bits, _/bits>> = Bitmap,
               Bit;
           false ->
               <<Bit:1/bits, _/bits>> = Bitmap,
               Bit
       end,

    %% Check if the bit is on.
    case Bit1 of
        <<1:1>> -> is_element(Key, Bitmap, T);
        <<0:1>> -> false
    end;
is_element(_, _, []) -> 
    true.

% This uses the "enhanced double hashing" algorithm.
% Todo: handle case of m > 2^32.
calc_idxs(Key) ->
    X = erlang:phash2(Key, ?M),
    Y = erlang:phash2({"salt", Key}, ?M),
    calc_idxs(?K - 1, X, Y, [X]).
calc_idxs(0, _, _, Acc) -> 
    Acc;
calc_idxs(I, X, Y, Acc) ->
    Xi = (X+Y) rem ?M,
    Yi = (Y+I) rem ?M,
    calc_idxs(I-1, Xi, Yi, [Xi | Acc]).

%% UNIT TESTS

-ifdef(TEST).

-ifdef(EQC).

prop_bloom_test_() ->
    {timeout, 60, fun() -> ?assert(eqc:quickcheck(prop_bloom())) end}.

g_keys() ->
    non_empty(list(non_empty(binary()))).

prop_bloom() ->
    ?FORALL(Keys, g_keys(),
            begin
                Bloom = ?MODULE:new(Keys),
                F = fun(X) -> is_element(X, Bloom) end,
                lists:all(F, Keys)
            end).

-endif.

-endif.
