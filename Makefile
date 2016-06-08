ERLOPTS= -pa . -DTEST
ERLC=erlc $(ERLOPTS)

all: server_fsm.beam netstat.beam server_index.beam server_sup.beam client_sup.beam client_fsm.beam grue_sup.beam player_fsm.beam

clean:
	rm -f *.beam

%.beam: %.erl
	$(ERLC) $<
