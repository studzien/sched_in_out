.PHONY = compile clean histograms run
GRAPHS = plusplus_slices.svg plusplus_rtt.svg append_rtt.svg append_slices.svg

graphs: $(GRAPHS)
run: compile histograms

compile:
	./rebar3 compile

clean:
	rm -rf *.csv *.svg _build

graphs: $(GRAPHS)

histograms: 
	erl -pa _build/default/lib/*/ebin -eval 'append_bench:start(), erlang:halt(0)'

%.svg: %.csv
	gnuplot -e "filename='$<'" tail-latency.gnuplot > $@
