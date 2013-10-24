%% grue499
%%
%% Copyright (c) 2013, Alex Wilson (arekinath)
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED  TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR  BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%

-module(player_fsm).

-behaviour(gen_fsm).

-export([start/4, init/1, handle_info/3, terminate/3]).
-export([connect/2, wait_banner/2, send_playergame/2, wait_hand/2, wait_bid/2, wait_play/2]).

start(Server, Tag, Rand, Port) ->
	gen_fsm:start(?MODULE, [Server, Tag, Rand, Port], []).

-record(state, {server, tag, port, sock, line, rand}).

init([Server, Tag, Rand, Port]) ->
	{ok, connect, #state{server = Server, rand = Rand, tag = Tag, port = Port, line = <<>>}, 0}.

-define(send(X), (S#state.server ! {S#state.tag, X})).

connect(timeout, S = #state{port = Port}) ->
	case gen_tcp:connect("localhost", Port, [binary, {active, true}]) of
		{ok, Sock} ->
			{next_state, wait_banner, S#state{sock = Sock}, 10000};
		_ ->
			?send(down),
			{stop, normal, S}
	end.

wait_banner({line, <<"M", _Banner/binary>>}, S = #state{sock = Sock}) ->
	{next_state, send_playergame, S, 0};
wait_banner({line, Other}, S = #state{sock = Sock}) ->
	gen_tcp:close(Sock),
	?send(protocol_err),
	{stop, normal, S};
wait_banner(disconnect, S = #state{}) ->
	?send(protocol_err),
	{stop, normal, S};
wait_banner(timeout, S = #state{sock = Sock}) ->
	gen_tcp:close(Sock),
	?send(timeout),
	{stop, normal, S}.

send_playergame({line, <<"M", _Msg/binary>>}, S = #state{}) ->
	{next_state, send_playergame, S, 0};
send_playergame({line, Other}, S = #state{sock = Sock}) ->
	?send(protocol_err),
	gen_tcp:close(Sock),
	{stop, normal, S};
send_playergame(disconnect, S = #state{}) ->
	?send(protocol_err),
	{stop, normal, S};
send_playergame(timeout, S = #state{sock = Sock, rand = Rand, tag = Tag}) ->
	TagBin = atom_to_binary(Tag, latin1),
	ok = gen_tcp:send(Sock, <<TagBin/binary, Rand/binary, "\n">>),
	ok = gen_tcp:send(Sock, <<"gruegame_", Rand/binary, "\n">>),
	?send(sent_playergame),
	{next_state, wait_hand, S, 20000}.

wait_hand({line, <<"M", _Msg/binary>>}, S = #state{}) ->
	{next_state, wait_hand, S, 10000};
wait_hand({line, <<"H", Hand/binary>>}, S = #state{tag = T}) ->
	?send({hand, Hand}),
	{next_state, wait_bid, S, 10000};
wait_hand({line, Other}, S = #state{sock = Sock}) ->
	?send(protocol_err),
	gen_tcp:close(Sock),
	{stop, normal, S};
wait_hand(disconnect, S = #state{}) ->
	?send(protocol_err),
	{stop, normal, S};
wait_hand(timeout, S = #state{sock = Sock}) ->
	?send(timeout),
	gen_tcp:close(Sock),
	{stop, normal, S}.

wait_bid({send, Msg}, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, Msg),
	{next_state, wait_bid, S};
wait_bid({line, <<"M", _Msg/binary>>}, S = #state{}) ->
	{next_state, wait_bid, S, 10000};
wait_bid({line, <<"B">>}, S = #state{}) ->
	?send(first_bid),
	{next_state, wait_bid, S};
wait_bid({line, <<"B", Bid/binary>>}, S = #state{}) ->
	?send({bid, Bid}),
	{next_state, wait_bid, S};
wait_bid({line, <<"T", Bid/binary>>}, S = #state{}) ->
	{next_state, wait_play, S, 10000};
wait_bid({line, Other}, S = #state{sock = Sock}) ->
	?send(protocol_err),
	gen_tcp:close(Sock),
	{stop, normal, S};
wait_bid(disconnect, S = #state{}) ->
	?send(protocol_err),
	{stop, normal, S};
wait_bid(timeout, S = #state{sock = Sock}) ->
	?send(timeout),
	gen_tcp:close(Sock),
	{stop, normal, S}.

wait_play({send, Msg}, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, Msg),
	{next_state, wait_play, S};
wait_play({line, <<"M", _Msg/binary>>}, S = #state{}) ->
	{next_state, wait_play, S, 10000};
wait_play({line, <<"L">>}, S = #state{}) ->
	?send(lead),
	{next_state, wait_play, S};
wait_play({line, <<"P", Follow/binary>>}, S = #state{}) ->
	?send({follow, Follow}),
	{next_state, wait_play, S};
wait_play({line, <<"A">>}, S = #state{}) ->
	?send(accept),
	{next_state, wait_play, S};
wait_play({line, Other}, S = #state{sock = Sock}) ->
	?send(protocol_err),
	gen_tcp:close(Sock),
	{stop, normal, S};
wait_play(disconnect, S = #state{}) ->
	?send(protocol_err),
	{stop, normal, S};
wait_play(timeout, S = #state{sock = Sock}) ->
	?send(timeout),
	gen_tcp:close(Sock),
	{stop, normal, S}.

terminate(Reason, State, _S = #state{port = Port, tag = Tag}) -> ok.

handle_info({tcp, Sock, Data}, State, S = #state{sock = Sock, line = L}) ->
	Line = <<L/binary, Data/binary>>,
	case binary:split(Line, <<"\n">>, [trim]) of
		[Line] when byte_size(Line) < 65535 ->
			{next_state, State, S#state{line = Line}};
		[NewLine] when byte_size(NewLine) < 65535 ->
			?MODULE:State({line, NewLine}, S#state{line = <<>>});
		[ChompLine, NewLine] ->
			self() ! {tcp, Sock, <<>>},
			?MODULE:State({line, ChompLine}, S#state{line = NewLine});
		_ ->
			?send(protocol_err),
			{stop, normal, S}
	end;
handle_info({tcp_closed, Sock}, State, S = #state{sock = Sock}) ->
	?MODULE:State(disconnect, S);

handle_info(Msg, State, S) ->
	?MODULE:State(Msg, S).
