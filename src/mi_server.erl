%% -------------------------------------------------------------------
%%
%% mi_server: main coordinator for merge_index process.
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

-module(mi_server).
-author("Rusty Klophaus <rusty@basho.com>").
-include("merge_index.hrl").

-export([
    buffer_to_segment/3,
    compact/1,
    drop/1,
    fold/3,
    has_deleteme_flag/1,
    index/2,
    info/4,
    is_empty/1,
    iterator/2,
    lookup/5,
    range/7,
    set_deleteme_flag/1,
    start_link/1,
    stop/1,
    %% GEN SERVER
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([lookup/8,
         range/10,
         iterate/6,
         iterate2/5]).

-record(state, {
    root,
    locks,
    segments,
    buffers,
    next_id,
    is_compacting,
    lookup_range_pids,
    buffer_rollover_size,
    converter,
    to_convert
}).

-record(stream_range, {
          pid,
          caller,
          ref,
          buffers,
          segments
         }).

-define(RESULTVEC_SIZE, 1000).
-define(DELETEME_FLAG, ".deleted").

%%%===================================================================
%%% API
%%%===================================================================

buffer_to_segment(Server, Buffer, SegmentWO) ->
    gen_server:cast(Server, {buffer_to_segment, Buffer, SegmentWO}).

compact(Server) -> gen_server:call(Server, start_compaction, infinity).

drop(Server) -> gen_server:call(Server, drop, infinity).

has_deleteme_flag(Filename) -> filelib:is_file(Filename ++ ?DELETEME_FLAG).

index(Server, Postings) -> gen_server:call(Server, {index, Postings}, infinity).

info(Server, Index, Field, Term) ->
    gen_server:call(Server, {info, Index, Field, Term}, infinity).

is_empty(Server) -> gen_server:call(Server, is_empty, infinity).

iterator(Server, Filter) ->
    Ref = make_ref(),
    gen_server:call(Server, {iterator, Filter, self(), Ref}, infinity),
    {ok, Ref}.

fold(Server, Fun, Acc) -> gen_server:call(Server, {fold, Fun, Acc}, infinity).

lookup(Server, Index, Field, Term, Filter) ->
    Ref = make_ref(),
    ok = gen_server:call(Server,
                         {lookup, Index, Field, Term, Filter, self(), Ref},
                         infinity),
    {ok, Ref}.

range(Server, Index, Field, StartTerm, EndTerm, Size, Filter) ->
    Ref = make_ref(),
    ok = gen_server:call(Server,
                         {range, Index, Field, StartTerm, EndTerm, Size,
                          Filter, self(), Ref},
                         infinity),
    {ok, Ref}.

set_deleteme_flag(Filename) ->
    file:write_file(Filename ++ ?DELETEME_FLAG, "").

start_link(Root) ->
    gen_server:start_link(mi_server, [Root], [{timeout, infinity}]).

stop(Server) ->
    gen_server:call(Server, stop).


%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Root]) ->
    lager:debug("loading merge_index '~s'", [Root]),
    %% Seed the random generator...
    Seed = {erlang:monotonic_time(), 
            erlang:time_offset(), 
            erlang:unique_integer()},
    _ = rand:seed(exs64, Seed ),

    %% Load from disk...
    filelib:ensure_dir(join(Root, "ignore")),
    {NextID, Buffer, Segments} = read_buf_and_seg(Root),

    %% trap exits so compaction and stream/range spawned processes
    %% don't pull down this merge_index if they fail
    process_flag(trap_exit, true),

    %% Create the state...
    State = #state {
        root     = Root,
        locks    = mi_locks:new(),
        buffers  = [Buffer],
        segments = Segments,
        next_id  = NextID,
        is_compacting = false,
        lookup_range_pids = [],
        buffer_rollover_size=fuzzed_rollover_size(),
        to_convert = queue:new()
    },

    lager:debug("finished loading merge_index '~s' with rollover size ~p",
               [Root, State#state.buffer_rollover_size]),
    {ok, State}.

handle_call({index, Postings}, _From, State) ->
    %% Write to the buffer...
    #state { buffers=[CurrentBuffer0|Buffers],
             converter=Converter,
             to_convert=ToConvert,
             root=Root} = State,

    %% By multiplying the timestamp by -1 and swapping order of TS and
    %% props, we can take advantage of the natural ordering of
    %% postings, eliminating the need for custom sort Functions (which
    %% makes things much faster.) We only need to do the
    %% multiplication here, because these values carry through to
    %% segments. We also group {Index, Field, Term} into a key because
    %% this is what mi_buffer needs to write to ets. This is a leaky
    %% abstraction for the benefit of speed.
    F = fun({Index, Field, Term, Value, Props, Tstamp}) ->
                {{Index, Field, Term}, Value, -1 * Tstamp, Props}
        end,
    Postings1 = [F(X) || X <- Postings],
    CurrentBuffer = mi_buffer:write(Postings1, CurrentBuffer0),

    %% Update the state...
    NewState = State#state {buffers = [CurrentBuffer | Buffers]},

    %% Possibly dump buffer to a new segment.
    case mi_buffer:filesize(CurrentBuffer) > State#state.buffer_rollover_size of
        true ->
            #state { next_id=NextID } = NewState,

            %% Close the buffer filehandle. Needs to be done in the owner process.
            mi_buffer:close_filehandle(CurrentBuffer),

            ToConvert2 = queue:in(CurrentBuffer, ToConvert),
            {Converter2, ToConvert4} =
                case Converter of
                    undefined ->
                        {Next, ToConvert3} = next_buffer_to_convert(ToConvert2),
                        {ok, Pid} = mi_buffer_converter:convert(self(),
                                                                Root,
                                                                Next),
                        {Pid, ToConvert3};
                    _ -> {Converter, ToConvert2}
                end,

            %% Create a new empty buffer...
            BName = join(NewState, "buffer." ++ integer_to_list(NextID)),
            NewBuffer = mi_buffer:new(BName),

            NewState1 = NewState#state {
                buffers=[NewBuffer|NewState#state.buffers],
                next_id=NextID + 1,
                buffer_rollover_size = fuzzed_rollover_size(),
                converter = Converter2,
                to_convert = ToConvert4
            },
            {reply, ok, NewState1};
        false ->
            {reply, ok, NewState}
    end;

handle_call(start_compaction, _From, State)
  when is_tuple(State#state.is_compacting) ->
    %% Don't compact if we are already compacting, or if we have fewer
    %% than five open segments.
    {reply, {ok, 0, 0}, State};

handle_call(start_compaction, From, State) ->
    %% Get list of segments to compact. Do this by getting filesizes,
    %% and then lopping off files larger than the average. This could be
    %% optimized with tuning, but probably a good enough solution.
    Segments = State#state.segments,
    {ok, MaxSegments} = application:get_env(merge_index, max_compact_segments),
    {ok, {M,F}} = application:get_env(merge_index, compact_mod_fun),
    SegmentsToCompact = case M:F(Segments) of
                            STC when length(STC) > MaxSegments ->
                                lists:sublist(STC, MaxSegments);
                            STC ->
                                STC
                        end,

    case SegmentsToCompact of
        [] ->
            {reply, {ok, 0, 0}, State};
        _ ->
            BytesToCompact = lists:sum([mi_segment:filesize(X) || X <- SegmentsToCompact]),

            %% Spawn a function to merge a bunch of segments into one...
            Pid = self(),
            CF =
                fun() ->
                        %% Create the group iterator...
                        SegmentIterators = [mi_segment:iterator(X) || X <- SegmentsToCompact],
                        GroupIterator = build_iterator_tree(SegmentIterators),

                        %% Create the new compaction segment...
                        <<MD5:128/integer>> = erlang:md5(term_to_binary({now, make_ref()})),
                        SName = join(State, io_lib:format("segment.~.16B", [MD5])),
                        set_deleteme_flag(SName),
                        CompactSegment = mi_segment:open_write(SName),

                        %% Run the compaction...
                        mi_segment:from_iterator(GroupIterator, CompactSegment),
                        gen_server:cast(Pid, {compacted, CompactSegment, SegmentsToCompact, BytesToCompact, From})
                end,
            CompactingPid = spawn_opt(CF, [link, {fullsweep_after, 0}]),
            {noreply, State#state { is_compacting={From, CompactingPid} }}
    end;

handle_call({info, Index, Field, Term}, _From, State) ->
    %% Calculate the IFT...
    #state { buffers=Buffers, segments=Segments } = State,

    %% Look up the weights in segments.
    BufferCount = [mi_buffer:info(Index, Field, Term, X) || X <- Buffers],
    SegmentCount = [mi_segment:info(Index, Field, Term, X) || X <- Segments],
    TotalCount = lists:sum([0|BufferCount]) + lists:sum([0|SegmentCount]),

    {reply, {ok, TotalCount}, State};

handle_call({lookup, Index, Field, Term, Filter, Pid, Ref}, _From, State) ->
    #state { locks=Locks, buffers=Buffers, segments=Segments } = State,

    NewLocks = lock_all(Locks, Buffers, Segments),
    LPid = spawn_link(?MODULE, lookup,
                      [Index, Field, Term, Filter, Pid, Ref,
                       Buffers, Segments]),

    NewPids = [ #stream_range{pid=LPid,
                              caller=Pid,
                              ref=Ref,
                              buffers=Buffers,
                              segments=Segments}
                | State#state.lookup_range_pids ],
    {reply, ok, State#state { locks=NewLocks, lookup_range_pids=NewPids }};

handle_call({range, Index, Field, StartTerm, EndTerm, Size, Filter, Pid, Ref},
            _From, State) ->
    #state { locks=Locks, buffers=Buffers, segments=Segments } = State,

    NewLocks = lock_all(Locks, Buffers, Segments),
    RPid = spawn_link(?MODULE, range,
                      [Index, Field, StartTerm, EndTerm, Size, Filter,
                       Pid, Ref, Buffers, Segments]),

    NewPids = [ #stream_range{pid=RPid,
                              caller=Pid,
                              ref=Ref,
                              buffers=Buffers,
                              segments=Segments}
                | State#state.lookup_range_pids ],
    {reply, ok, State#state { locks=NewLocks, lookup_range_pids=NewPids }};

%% NOTE: The order in which fold returns postings is not deterministic
%% and is determined by things such as buffer_rollover_size.
handle_call({fold, FoldFun, Acc}, _From, State) ->
    #state { buffers=Buffers, segments=Segments } = State,

    %% Wrap the FoldFun so that we have a chance to do IndexID /
    %% FieldID / TermID lookups
    WrappedFun = fun({Index, Field, Term, Value, TS, Props}, AccIn) ->
        %% Call the fold function. Undo the Timestamp inversion.
        FoldFun(Index, Field, Term, Value, Props, -1 * TS, AccIn)
    end,

    %% Fold through all the buffers...
    F1 = fun(Buffer, AccIn) ->
                 Iterator = mi_buffer:iterator(Buffer),
                 fold_itr(WrappedFun, AccIn, Iterator())
         end,
    Acc1 = lists:foldl(F1, Acc, Buffers),

    %% Fold through all the segments...
    F2 = fun(Segment, AccIn) ->
                 Iterator = mi_segment:iterator(Segment),
                 fold_itr(WrappedFun, AccIn, Iterator())
         end,
    Acc2 = lists:foldl(F2, Acc1, Segments),

    %% Reply...
    {reply, {ok, Acc2}, State};

handle_call(is_empty, _From, State) ->
    %% Check if we have buffer data...
    HasBufferData = 
      case State#state.buffers of
         [] ->
            false;
         [Buffer] ->
            mi_buffer:size(Buffer) > 0;
         _ ->
            true
    end,

    %% Check if we have segment data.
    HasSegmentData = length(State#state.segments) > 0,

    %% Return.
    IsEmpty = (not HasBufferData) andalso (not HasSegmentData),
    {reply, IsEmpty, State};

handle_call({iterator, Filter, DestPid, DestRef}, _From, State) ->
    #state { locks=Locks, buffers=Buffers, segments=Segments } = State,

    NewLocks = lock_all(Locks, Buffers, Segments),

    %% build ordered iterator over all buffers + segments
    BufferItrs = [mi_buffer:iterator(B) || B <- Buffers],
    SegmentItrs = [mi_segment:iterator(S) || S <- Segments],
    Itr = build_iterator_tree(BufferItrs ++ SegmentItrs),

    ItrPid = spawn_link(
               fun() ->
                       iterate2(Filter, DestPid, DestRef, Itr(), {[],0})
               end),

    NewPids = [ #stream_range{pid=ItrPid,
                              caller=DestPid,
                              ref=DestRef,
                              buffers=Buffers,
                              segments=Segments}
                | State#state.lookup_range_pids ],

    State2 = State#state{locks=NewLocks, lookup_range_pids=NewPids},
    {reply, {ok, Itr}, State2};

%% TODO what about resetting next_id?
handle_call(drop, _From, State) ->
    #state { buffers=Buffers, segments=Segments } = State,

    %% Delete files, reset state...
    [mi_buffer:delete(X) || X <- Buffers],
    [mi_segment:delete(X) || X <- Segments],
    BufferFile = join(State, "buffer.1"),
    Buffer = mi_buffer:new(BufferFile),
    NewState = State#state { locks = mi_locks:new(),
                             buffers = [Buffer],
                             segments = [],
                             converter = undefined,
                             to_convert = queue:new()},
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    lager:error("Unexpected call ~p", [Msg]),
    {reply, ok, State}.

handle_cast({compacted, CompactSegmentWO, OldSegments, OldBytes, From}, State) ->
    #state { locks=Locks, segments=Segments } = State,

    %% Clean up. Remove delete flag on the new segment. Add delete
    %% flags to the old segments. Register to delete the old segments
    %% when the locks are freed.
    clear_deleteme_flag(mi_segment:filename(CompactSegmentWO)),

    %% Open the segment as read only...
    CompactSegmentRO = mi_segment:open_read(mi_segment:filename(CompactSegmentWO)),

    [set_deleteme_flag(mi_segment:filename(X)) || X <- OldSegments],
    F = fun(X, Acc) ->
        mi_locks:when_free(mi_segment:filename(X), fun() -> mi_segment:delete(X) end, Acc)
    end,
    NewLocks = lists:foldl(F, Locks, OldSegments),

    %% Update State and return...
    NewState = State#state {
        locks=NewLocks,
        segments=[CompactSegmentRO|(Segments -- OldSegments)],
        is_compacting=false
    },

    %% Tell the awaiting process that we've finished compaction.
    gen_server:reply(From, {ok, length(OldSegments), OldBytes}),
    {noreply, NewState};

handle_cast({buffer_to_segment, Buffer, SegmentWO}, State) ->
    #state { root=Root, locks=Locks, buffers=Buffers, segments=Segments,
             to_convert=ToConvert,
             is_compacting=IsCompacting } = State,

    %% Clean up by clearing delete flag on the segment, adding delete
    %% flag to the buffer, and telling the system to delete the buffer
    %% as soon as the last lock is released.
    case lists:member(Buffer, Buffers) of
        true ->
            clear_deleteme_flag(mi_segment:filename(SegmentWO)),
            BName = mi_buffer:filename(Buffer),
            set_deleteme_flag(BName),
            NewLocks = mi_locks:when_free(BName,
                                          fun() ->
                                                  mi_buffer:delete(Buffer)
                                          end, Locks),

            %% Open the segment as read only...
            SegmentRO = mi_segment:open_read(mi_segment:filename(SegmentWO)),

            {{value, _}, ToConvert2} = queue:out(ToConvert),
            {Converter, ToConvert3} =
                case next_buffer_to_convert(ToConvert2) of
                    {none, ToConvert2} -> {undefined, ToConvert2};
                    {Next, ToConvert2} ->
                        {ok, Pid} = mi_buffer_converter:convert(self(),
                                                                Root,
                                                                Next),
                        {Pid, ToConvert2}
                end,

            %% Update state...
            NewSegments = [SegmentRO|Segments],
            NewState = State#state {
                         locks=NewLocks,
                         buffers=Buffers -- [Buffer],
                         segments=NewSegments,
                         converter=Converter,
                         to_convert=ToConvert3
                        },

            %% Give us the opportunity to do a merge...
            {ok, {M,F}} = application:get_env(merge_index, compact_mod_fun),
            SegmentsToMerge = M:F(NewSegments),
            case length(SegmentsToMerge) of
                Num when Num =< 2 orelse is_tuple(IsCompacting) ->
                    ok;
                _ ->
                    mi_scheduler:schedule_compaction(self())
            end,
            {noreply, NewState};
        false ->
            lager:warning("`buffer_to_segment` cast received"
                          " for nonexistent buffer, probably"
                          " because drop was called"),
            {noreply, State}
    end;

handle_cast(Msg, State) ->
    lager:error("Unexpected cast ~p", [Msg]),
    {noreply, State}.

handle_info({'EXIT', CompactingPid, Reason},
            #state{is_compacting={From, CompactingPid}}=State) ->
    %% the spawned compaction process exited
    case Reason of
        normal ->
            %% compaction finished normally: nothing to be done
            %% handle_call({compacted... already sent the reply
            ok;
        _ ->
            %% compaction failed: not too much to worry about
            %% (it should be safe to try again later)
            %% but we need to let the compaction-requester know
            %% that we're not compacting any more
            gen_server:reply(From, {error, Reason})
    end,

    %% clear out compaction flags, so we try again when necessary
    {noreply, State#state{is_compacting=false}};

handle_info({'EXIT', Pid, Reason},
            #state{lookup_range_pids=SRPids}=State) ->

    case lists:keytake(Pid, #stream_range.pid, SRPids) of
        {value, SR, NewSRPids} ->
            %% One of our lookup or range processes exited

            case Reason of
                normal ->
                    SR#stream_range.caller ! {eof, SR#stream_range.ref};
                _ ->
                    lager:error("lookup/range failure: ~p", [Reason]),
                    SR#stream_range.caller
                        ! {error, SR#stream_range.ref, Reason}
            end,

            %% Remove locks from all buffers...
            F1 = fun(Buffer, Acc) ->
                mi_locks:release(mi_buffer:filename(Buffer), Acc)
            end,
            NewLocks = lists:foldl(F1, State#state.locks,
                                   SR#stream_range.buffers),

            %% Remove locks from all segments...
            F2 = fun(Segment, Acc) ->
                mi_locks:release(mi_segment:filename(Segment), Acc)
            end,
            NewLocks1 = lists:foldl(F2, NewLocks,
                                   SR#stream_range.segments),

            {noreply, State#state { locks=NewLocks1,
                                    lookup_range_pids=NewSRPids }};
        false ->
            %% some random other process exited: ignore
            {noreply, State}
    end;

handle_info(Msg, State) ->
    lager:error("Unexpected info ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal
%%%===================================================================

%% Return {Buffers, Segments}, after cleaning up/repairing any partially merged buffers.
read_buf_and_seg(Root) ->
    %% Delete any files that have a ?DELETEME_FLAG flag. This means that
    %% the system stopped before proper cleanup.
    F1 = fun(Filename) ->
        Basename = filename:basename(Filename, ?DELETEME_FLAG),
        Basename1 = filename:join(Root, Basename ++ ".*"),
        lager:debug("deleting '~s'", [Basename1]),
        [ok = file:delete(X) || X <- filelib:wildcard(Basename1)]
    end,
    [F1(X) || X <- filelib:wildcard(join(Root, "*.deleted"))],

    %% Open the segments...
    SegmentFiles = filelib:wildcard(join(Root, "segment.*.data")),
    SegmentFiles1 = [filename:join(Root, filename:basename(X, ".data")) || X <- SegmentFiles],
    Segments = read_segments(SegmentFiles1, []),

    %% Get buffer files, calculate the next_id, load the buffers, turn
    %% any extraneous buffers into segments...
    BufferFiles = filelib:wildcard(join(Root, "buffer.*")),
    BufferFiles1 = lists:sort([{mi_buffer:id(X), X} || X <- BufferFiles]),
    NextID = lists:max([X || {X, _} <- BufferFiles1] ++ [0]) + 1,
    {NextID1, Buffer, Segments1} = read_buffers(Root, BufferFiles1, NextID, Segments),

    %% Return...
    {NextID1, Buffer, Segments1}.

read_segments([], _Segments) -> [];
read_segments([SName|Rest], Segments) ->
    %% Read the segment from disk...
    lager:debug("opening segment: '~s'", [SName]),
    Segment = mi_segment:open_read(SName),
    [Segment|read_segments(Rest, Segments)].

read_buffers(Root, [], NextID, Segments) ->
    %% No latest buffer exists, open a new one...
    BName = join(Root, "buffer." ++ integer_to_list(NextID)),
    Buffer = mi_buffer:new(BName),
    {NextID + 1, Buffer, Segments};

read_buffers(_Root, [{_BNum, BName}], NextID, Segments) ->
    %% This is the final buffer file... return it as the open buffer...
    Buffer = mi_buffer:new(BName),
    {NextID, Buffer, Segments};

read_buffers(Root, [{BNum, BName}|Rest], NextID, Segments) ->
    %% Multiple buffers exist... convert them into segments...
    lager:debug("converting buffer: '~s' to segment", [BName]),
    SName = join(Root, "segment." ++ integer_to_list(BNum)),
    set_deleteme_flag(SName),
    Buffer = mi_buffer:new(BName),
    mi_buffer:close_filehandle(Buffer),
    SegmentWO = mi_segment:open_write(SName),
    mi_segment:from_buffer(Buffer, SegmentWO),
    mi_buffer:delete(Buffer),
    clear_deleteme_flag(mi_segment:filename(SegmentWO)),
    SegmentRO = mi_segment:open_read(SName),

    %% Loop...
    read_buffers(Root, Rest, NextID, [SegmentRO|Segments]).

%% Merge-sort the results from Iterators, and stream to the pid.
lookup(Index, Field, Term, Filter, Pid, Ref, Buffers, Segments) ->
    BufferIterators = [mi_buffer:iterator(Index, Field, Term, X) || X <- Buffers],
    SegmentIterators = [mi_segment:iterator(Index, Field, Term, X) || X <- Segments],
    GroupIterator = build_iterator_tree(BufferIterators ++ SegmentIterators),

    iterate(Filter, Pid, Ref, undefined, GroupIterator(), []),
    ok.

range(Index, Field, StartTerm, EndTerm, Size, Filter, Pid, Ref,
      Buffers, Segments) ->
    BufferIterators = lists:flatten([mi_buffer:iterators(Index, Field, StartTerm, EndTerm, Size, X) || X <- Buffers]),
    SegmentIterators = lists:flatten([mi_segment:iterators(Index, Field, StartTerm, EndTerm, Size, X) || X <- Segments]),
    GroupIterator = build_iterator_tree(BufferIterators ++ SegmentIterators),

    iterate(Filter, Pid, Ref, undefined, GroupIterator(), []),
    ok.

iterate(_Filter, Pid, Ref, LastValue, Iterator, Acc)
  when length(Acc) > ?RESULTVEC_SIZE ->
    Pid ! {results, lists:reverse(Acc), Ref},
    iterate(_Filter, Pid, Ref, LastValue, Iterator, []);
iterate(Filter, _Pid, _Ref, LastValue,
                      {{Value, _TS, Props}, Iter}, Acc) ->
    %% TODO: Ideally, dedup should happen a layer above, as noted in
    %% the following issue.
    %%
    %% https://issues.basho.com/show_bug.cgi?id=1099
    IsDuplicate = (LastValue == Value),
    IsDeleted = (Props == undefined),
    case (not IsDuplicate) andalso (not IsDeleted)
        andalso Filter(Value, Props) of
        true  ->
            iterate(Filter, _Pid, _Ref, Value, Iter(), [{Value, Props}|Acc]);
        false ->
            iterate(Filter, _Pid, _Ref, Value, Iter(), Acc)
    end;
iterate(_, Pid, Ref, _, eof, Acc) ->
    Pid ! {results, lists:reverse(Acc), Ref},
    ok.

%% This is currently a copy/paste of iterate with specific changes
%% noted below.
%%
%% Don't dedup here as this is being used for async folding that drives
%% handoff/repair.  The reason dedup is bad here is because for the
%% purpose of handoff/repair you want _all_ IFTs for a given Value.
%%
%% Don't check if deleted because tombstones need to be replicate or
%% else indexes will reappear when they shouldn't.
iterate2(_Filter, Pid, Ref, _Iterator, {_Results, 4}) ->
    Pid ! {waiting, self(), Ref},
    receive
        {continue, Ref} ->
            iterate2(_Filter, Pid, Ref, _Iterator, {_Results, 0})
    after 60000 ->
            throw({error, iterate2, timeout_waiting_for_continue})
    end;
iterate2(_Filter, Pid, Ref, Iterator, {Results, MsgCount})
  when length(Results) > ?RESULTVEC_SIZE ->
    Pid ! {results, lists:reverse(Results), Ref},
    iterate2(_Filter, Pid, Ref, Iterator, {[], MsgCount+1});
iterate2(Filter, _Pid, _Ref, {{I, F, T, Value, TS, Props}, Iter},
         {Results, _MsgCount}) ->
    case Filter(Value, Props) of
        true  ->
            V = {I, F, T, Value, -1 * TS, Props},
            iterate2(Filter, _Pid, _Ref, Iter(), {[V|Results], _MsgCount});
        false ->
            iterate2(Filter, _Pid, _Ref, Iter(), {Results, _MsgCount})
    end;
iterate2(_, Pid, Ref, eof, {Results, _MsgCount}) ->
    Pid ! {results, lists:reverse(Results), Ref},
    ok.

%% Chain a list of iterators into what looks like one single
%% iterator.
build_iterator_tree([]) ->
    fun() -> eof end;
build_iterator_tree(Iterators) ->
    case build_iterator_tree_inner(Iterators) of
        [OneIterator] -> OneIterator;
        ManyIterators -> build_iterator_tree(ManyIterators)
    end.
build_iterator_tree_inner([]) ->
    [];
build_iterator_tree_inner([Iterator]) ->
    [Iterator];
build_iterator_tree_inner([IteratorA,IteratorB|Rest]) ->
    Iterator = fun() -> group_iterator(IteratorA(), IteratorB()) end,
    [Iterator|build_iterator_tree_inner(Rest)].

%% group_iterator_term/2 - Combine two iterators into one iterator.
group_iterator(I1 = {Term1, Iterator1}, I2 = {Term2, Iterator2}) ->
    case Term1 < Term2 of
        true ->
            NewIterator = fun() -> group_iterator(Iterator1(), I2) end,
            {Term1, NewIterator};
        false ->
            NewIterator = fun() -> group_iterator(I1, Iterator2()) end,
            {Term2, NewIterator}
    end;
group_iterator(eof, eof) ->
    eof;
group_iterator(eof, Iterator) ->
    Iterator;
group_iterator(Iterator, eof) ->
    Iterator.

clear_deleteme_flag(Filename) ->
    file:delete(Filename ++ ?DELETEME_FLAG).

fold_itr(_Fun, Acc, eof) -> Acc;
fold_itr(Fun, Acc, {Term, IteratorFun}) ->
    fold_itr(Fun, Fun(Term, Acc), IteratorFun()).

join(#state { root=Root }, Name) ->
    join(Root, Name);

join(Root, Name) ->
    filename:join([Root, Name]).

%% Add some random variation (plus or minus 25%) to the rollover size
%% so that we don't get all buffers rolling over at the same time.
fuzzed_rollover_size() ->
    ActualRolloverSize = element(2,application:get_env(merge_index, buffer_rollover_size)),
    mi_utils:fuzz(ActualRolloverSize, 0.25).

next_buffer_to_convert(Buffers) ->
    case queue:peek(Buffers) of
        {value, Next} ->
            %% Buffer may have since been dropped via merge_index:drop
            case mi_buffer:exists(Next) of
                true -> {Next, Buffers};
                false ->
                    {_, Buffers2} = queue:out(Buffers),
                    next_buffer_to_convert(Buffers2)
            end;
        empty -> {none, Buffers}
    end.

buffer_filenames(Buffers) ->
    [mi_buffer:filename(Buffer) || Buffer <- Buffers].

segment_filenames(Segments) ->
    [mi_segment:filename(Segment) || Segment <- Segments].

lock_all(Locks, Buffers, Segments) ->
    BufferNames = buffer_filenames(Buffers),
    SegmentNames = segment_filenames(Segments),
    mi_locks:claim_many(BufferNames ++ SegmentNames, Locks).
