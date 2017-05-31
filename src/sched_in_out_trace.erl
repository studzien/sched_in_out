-module(sched_in_out_trace).

-compile(export_all).

-behaviour(gen_server).

start_link(Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Filename, []).

stop() ->
    gen_server:call(?MODULE, stop).

init(Filename) ->
    erlang:trace(new, true, [running, running_ports, timestamp]),
    Ins = ets:new(ins, [set]),
    {ok, Histogram} = hdr_histogram:open(10000000, 2),
    {ok, #{histogram => Histogram, ins => Ins, filename => Filename}}.

handle_info({trace_ts, Pid, in, _, TsIn}, #{ins := Ins}=State) ->
    true = ets:insert(Ins, {Pid, TsIn}),
    {noreply, State};
handle_info({trace_ts, Pid, out, _, TsOut},
            #{histogram := H, ins := Ins} = State) ->
    [{Pid, TsIn}] = ets:lookup(Ins, Pid),
    hdr_histogram:record(H, timer:now_diff(TsOut, TsIn)),
    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

terminate(_Reason, #{histogram := H, filename := Filename}) ->
    hdr_histogram:log(H, csv, Filename).
