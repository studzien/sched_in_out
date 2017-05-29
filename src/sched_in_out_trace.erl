-module(sched_in_out_trace).

-compile(export_all).

-behaviour(gen_server).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    erlang:trace(all, true, [running, running_ports, timestamp]),
    {ok, Histogram} = hdr_histogram:open(10000000, 2),
    {ok, #{histogram => Histogram}}.

handle_info({trace_ts, Pid, in, _, TsIn}, State) ->
    {noreply, maps:put(Pid, TsIn, State)};
handle_info({trace_ts, Pid, out, _, TsOut}, #{histogram := H} = State) ->
    TsIn = maps:get(Pid, State),
    hdr_histogram:record(H, timer:now_diff(TsOut, TsIn)),
    {noreply, maps:remove(Pid, State)}.

terminate(_Reason, #{histogram := H}) ->
    hdr_histogram:log(H, csv, "in_out.csv").
