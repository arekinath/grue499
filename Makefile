ERLOPTS= -pa . -DTEST
ERLC=erlc $(ERLOPTS)

all: server_fsm.beam netstat.beam server_index.beam server_sup.beam client_sup.beam client_fsm.beam grue_sup.beam player_fsm.beam

clean:
	rm -f *.beam

server_fsm.beam: server_fsm.erl
	$(ERLC) server_fsm.erl

server_index.beam: server_index.erl
	$(ERLC) server_index.erl

server_sup.beam: server_sup.erl
	$(ERLC) server_sup.erl

netstat.beam: netstat.erl
	$(ERLC) netstat.erl

client_sup.beam: client_sup.erl
	$(ERLC) client_sup.erl

client_fsm.beam: client_fsm.erl
	$(ERLC) client_fsm.erl

player_fsm.beam: player_fsm.erl
	$(ERLC) player_fsm.erl

grue_sup.beam: grue_sup.erl
	$(ERLC) grue_sup.erl
