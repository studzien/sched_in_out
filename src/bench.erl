-module(bench).

-compile(export_all).

-define(PROCS, 8).
-define(MESSAGES, 1000).

start(Fun, Filename) ->
    random:seed(os:timestamp()),
    Histograms = histograms(?PROCS),
    Lists = [{list1(100),    list2(100)},
             {list1(1000),   list2(1000)},
             {list1(10000),  list2(10000)},
             {list1(100000), list2(100000)},
             {list1(1000000),list2(1000000)}],
    Workers = [ spawn(?MODULE, do, [Fun, Lists, []])
                || _ <- lists:seq(1, ?PROCS) ],
    Servers = [ spawn_link(?MODULE, echo_server, [])
                    || _ <- lists:seq(1, ?PROCS) ],
    timer:sleep(1000),
    Senders = [ spawn_link(?MODULE, sender, [self(), S, H, ?MESSAGES])
                || {S, H} <- lists:zip(Servers, Histograms) ],
    ok = wait_for_completion(Senders),
    [ exit(Pid, kill) || Pid <- Workers ],
    AggregatedHistogram = aggregate_histograms(Histograms),
    hdr_histogram:log(AggregatedHistogram, csv, Filename).

list1(N) -> [{I, I+1} || I <- lists:seq(1, N)].
list2(N) -> [{I, I+2} || I <- lists:seq(1, N)].

wait_for_completion([]) -> ok;
wait_for_completion([H|T]) ->
    receive {H, done} -> wait_for_completion(T) end.

echo_server() ->
    receive {From, Msg} -> From ! {echo, Msg} end,
    echo_server().

sender(Parent, _, _, 0) -> Parent ! {self(), done};
sender(Parent, Server, Histogram, N) ->
    {Time, ok} = timer:tc(fun() ->
                                  Ref = make_ref(),
                                  Server ! {self(), Ref},
                                  receive {echo, Ref} -> ok end
                          end),
    hdr_histogram:record(Histogram, Time),
    sender(Parent, Server, Histogram, N-1).

do(Fun, Lists, _Acc) ->
    R = [ Fun(List1, List2) || {List1, List2} <- shuffle(Lists, []) ],
    do(Fun, Lists, R).

shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    I = random:uniform(length(List)),
    El = lists:nth(I, List),
    shuffle(lists:delete(El, List), [El|Acc]).

histograms(N) ->
    [ begin {ok, H} = hdr_histogram:open(1000000, 2), H end
      || _ <- lists:seq(1, N) ].

aggregate_histograms([H|Hs]) ->
    lists:foldl(fun(To, From) -> hdr_histogram:add(To, From), To end, H, Hs).
