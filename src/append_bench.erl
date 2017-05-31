-module(append_bench).

-compile(export_all).

start() ->
    io:format("Running ++ benchmark...~n"),
    {ok, _} = sched_in_out_trace:start_link("plusplus_slices.csv"),
    bench:start(fun(L1, L2) -> L1 ++ L2 end, "plusplus_rtt.csv"),
    ok = sched_in_out_trace:stop(),
    io:format("Done...~n"),

    io:format("Running append benchmark...~n"),
    {ok, _} = sched_in_out_trace:start_link("append_slices.csv"),
    bench:start(fun(L1, L2) -> append(L1, L2) end, "append_rtt.csv"),
    ok = sched_in_out_trace:stop(),
    io:format("Done...~n").

append(L1, L2) -> do_append(L2, lists:reverse(L1)).

do_append([], Acc) -> lists:reverse(Acc);
do_append([H|T], Acc) -> do_append(T, [H|Acc]).

