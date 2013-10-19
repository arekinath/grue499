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

-module(server_fsm).

-behaviour(gen_fsm).

-export([start_link/1, info/1, init/1, handle_info/3, terminate/3]).
-export([connect/2, wait_banner/2, send_playergame/2, wait_extra/2, deal_test/2, play_test/2]).

start_link(Port) ->
	gen_fsm:start_link(?MODULE, [Port], []).

info(Ref) ->
	case ets:lookup(server_status, Ref) of
		[{Ref, Banner, Flags}] -> {ok, Banner, Flags};
		_ -> {error, not_found}
	end.

-record(state, {port, sock, line, banner, flags, chances = 30, players = [], runner, last_pid}).

update_ets(S = #state{banner = B, flags = Fl}) ->
	ets:insert(server_status, {self(), B, gb_sets:to_list(Fl)}),
	S.

init([Port]) ->
	process_flag(trap_exit, true),
	{ok, connect, update_ets(#state{port = Port, line = <<>>, banner = <<>>, flags = gb_sets:new()}), 1000}.

connect(timeout, S = #state{port = Port, chances = Ch, flags = FlIn, last_pid = LastPid}) ->
	[{Port, Pid, _}] = ets:lookup(port_pid, Port),
	Fl = case Pid of
		LastPid -> FlIn;
		_ -> gb_sets:new()
	end,
	case gen_tcp:connect("localhost", Port, [binary, {active, true}]) of
		{ok, Sock} ->
			Fl2 = gb_sets:del_element(down, Fl),
			{next_state, wait_banner, update_ets(S#state{sock = Sock, flags = Fl2, chances = 30, last_pid = Pid}), 10000};
		_ ->
			if (Ch > 0) ->
				Fl2 = gb_sets:add_element(down, Fl),
				{next_state, connect, update_ets(S#state{chances = Ch - 1, flags = Fl2, last_pid = Pid}), 20000};
			true ->
				{stop, normal, S}
			end
	end.

wait_banner({line, <<"M", Banner/binary>>}, S = #state{sock = Sock, flags = Fl}) ->
	Fl2 = gb_sets:add_element(has_banner, Fl),
	{next_state, send_playergame, update_ets(S#state{banner = Banner, flags = Fl2}), 0};
wait_banner({line, Other}, S = #state{sock = Sock}) ->
	gen_tcp:close(Sock),
	{next_state, connect, S, 60000};
wait_banner(disconnect, S = #state{flags = Fl}) ->
	{next_state, connect, S, 30000};
wait_banner(timeout, S = #state{sock = Sock}) ->
	gen_tcp:close(Sock),
	{next_state, connect, S, 30000}.

send_playergame({line, <<"M", Msg/binary>>}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(extra_msg, Fl),
	{next_state, send_playergame, update_ets(S#state{flags = Fl2}), 0};
send_playergame({line, Other}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(protocol_err, Fl),
	{next_state, send_playergame, update_ets(S#state{flags = Fl2}), 0};
send_playergame(timeout, S = #state{sock = Sock}) ->
	Bytes = base64:encode(crypto:rand_bytes(9)),
	gen_tcp:send(Sock, <<"grue", Bytes/binary, "\n">>),
	gen_tcp:send(Sock, <<"gruegame", Bytes/binary, "\n">>),
	{next_state, wait_extra, S, 10000}.

wait_extra({line, <<"M", Msg/binary>>}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(extra_msg, Fl),
	{next_state, wait_extra, update_ets(S#state{flags = Fl2}), 10000};
wait_extra({line, Other}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(protocol_err, Fl),
	{next_state, wait_extra, update_ets(S#state{flags = Fl2}), 10000};
wait_extra(disconnect, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(disconnected_after_name, Fl),
	{next_state, connect, update_ets(S#state{flags = Fl2}), 30000};
wait_extra(timeout, S = #state{sock = Sock, flags = Fl}) ->
	Fl2 = gb_sets:add_element(can_give_name, Fl),
	gen_tcp:close(Sock),
	{next_state, deal_test, update_ets(S#state{flags = Fl2}), 5000}.

drain_player_msg() ->
	receive
		{p1, _} -> drain_player_msg();
		{p2, _} -> drain_player_msg();
		{p3, _} -> drain_player_msg();
		{p4, _} -> drain_player_msg()
	after 100 -> ok
	end.

kill_players(S) ->
	[P1, P2, P3, P4] = S#state.players,
	P1 ! timeout, receive {p1, timeout} -> ok after 5000 -> ok end,
	P2 ! timeout, receive {p2, timeout} -> ok after 5000 -> ok end,
	P3 ! timeout, receive {p3, timeout} -> ok after 5000 -> ok end,
	P4 ! timeout, receive {p4, timeout} -> ok after 5000 -> ok end,
	drain_player_msg().

deal_test(timeout, S = #state{port = Port}) ->
	Bytes = base64:encode(crypto:rand_bytes(9)),
	Players = [P || {ok, P} <- [player_fsm:start(self(), A, Bytes, Port) || A <- [p1,p2,p3,p4]]],
	{next_state, deal_test, S#state{players = Players}};
deal_test({_, down}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(disconnected_before_deal, Fl),
	kill_players(S),
	{next_state, connect, update_ets(S#state{flags = Fl2}), 30000};
deal_test({_, timeout}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(deal_timeout, Fl),
	kill_players(S),
	{next_state, connect, update_ets(S#state{flags = Fl2}), 30000};
deal_test({_, protocol_err}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(protocol_err, Fl),
	kill_players(S),
	{next_state, connect, update_ets(S#state{flags = Fl2}), 30000};
deal_test({_, {hand, H}}, S = #state{flags = Fl}) ->
	if (byte_size(H) == 26) ->
		Fl2 = gb_sets:add_element(deals_hands, Fl),
		{next_state, deal_test, update_ets(S#state{flags = Fl2})};
	true ->
		Fl2 = gb_sets:add_element(dealt_bad_hand, Fl),
		{next_state, deal_test, update_ets(S#state{flags = Fl2})}
	end;
deal_test({p1, first_bid}, S = #state{flags = Fl, players = [P1 | _]}) ->
	Fl2 = gb_sets:add_element(first_bid_ok, Fl),
	P1 ! {send, <<"4C\n">>},
	{next_state, deal_test, update_ets(S#state{flags = Fl2})};
deal_test({p2, {bid, <<"4C">>}}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(second_bid_ok, Fl),
	kill_players(S),
	{next_state, play_test, update_ets(S#state{flags = Fl2}), 5000};
deal_test({p1, _}, S) -> {next_state, deal_test, S};
deal_test({p2, _}, S) -> {next_state, deal_test, S};
deal_test({p3, _}, S) -> {next_state, deal_test, S};
deal_test({p4, _}, S) -> {next_state, deal_test, S}.

hand_split(<<>>) -> [];
hand_split(<<Rank, Suit, Rest/binary>>) ->
	[[Rank, Suit] | hand_split(Rest)].

play_test(timeout, S = #state{port = Port}) ->
	Me = self(),
	Runner = spawn_link(fun() ->
		timer:kill_after(30000),

		Players = receive {players, PP} -> PP after 5000 -> error(no_players) end,
		[P1,P2,P3,P4] = Players,
		[P1H,P2H,P3H,P4H] = [receive {P, {hand, H}} -> hand_split(H) after 5000 -> error(no_hand) end || P <- [p1,p2,p3,p4]],

		receive {p1, first_bid} -> ok after 5000 -> error(no_first_bid) end,
		drain_player_msg(),

		P1 ! {send, <<"5C\n">>},
		receive {p2, {bid, <<"5C">>}} -> ok end,
		P2 ! {send, <<"4C\n">>},
		receive {p2, {bid, <<"5C">>}} -> ok end,
		P2 ! {send, <<"6C\n">>},
		receive {p3, {bid, <<"6C">>}} -> ok end,
		P3 ! {send, <<"PP\n">>},
		receive {p4, {bid, <<"6C">>}} -> ok end,
		P4 ! {send, <<"PP\n">>},
		receive {p1, {bid, <<"6C">>}} -> ok end,
		drain_player_msg(),

		P1 ! {send, <<"PP\n">>},
		receive {p2, lead} -> ok after 5000 -> error(no_lead) end,
		Me ! {set_flag, full_bidding},

		[P2Play1 | P2H2] = P2H,
		[_, Suit] = P2Play1,
		SuitBin = <<Suit>>,
		P2 ! {send, [P2Play1,<<"\n">>]},
		receive {p2, accept} -> ok after 5000 -> error(no_accept) end,

		receive {p3, {follow, SuitBin}} -> ok after 5000 -> error(no_follow) end,
		Me ! {set_flag, play_cards},

		Me ! play_test_done
	end),
	Bytes = base64:encode(crypto:rand_bytes(9)),
	Players = [P || {ok, P} <- [player_fsm:start(Runner, A, Bytes, Port) || A <- [p1,p2,p3,p4]]],
	Runner ! {players, Players},
	{next_state, play_test, S#state{players = Players, runner = Runner}};
play_test({'EXIT', Pid, Reason}, S = #state{flags = Fl, runner = Pid}) ->
	Fl2 = gb_sets:add_element(play_test_timeout, Fl),
	kill_players(S),
	{next_state, connect, update_ets(S#state{flags = Fl2}), 30000};
play_test({set_flag, F}, S = #state{flags = Fl}) ->
	Fl2 = gb_sets:add_element(F, Fl),
	{next_state, play_test, update_ets(S#state{flags = Fl2}), 30000};
play_test(play_test_done, S = #state{runner = Pid}) ->
	kill_players(S),
	receive {'EXIT', Pid, normal} -> ok end,
	{next_state, connect, update_ets(S), 30000}.

terminate(_Reason, _State, _S = #state{port = _Port}) -> ok.

handle_info({tcp, Sock, Data}, State, S = #state{sock = Sock, line = L, flags = Fl}) ->
	Line = <<L/binary, Data/binary>>,
	case binary:last(Data) of
		$\n ->
			ChompLine = binary:part(Line, 0, byte_size(Line)-1),
			?MODULE:State({line, ChompLine}, S#state{line = <<>>});
		_ ->
			if byte_size(Line) > 65535 ->
				gen_tcp:close(Sock),
				{next_state, connect, S#state{flags = gb_sets:add_element(longline, Fl)}, 30000};
			true ->
				{next_state, State, S#state{line = Line}}
			end
	end;
handle_info({tcp_closed, Sock}, State, S = #state{sock = Sock}) ->
	?MODULE:State(disconnect, S);

handle_info(Msg, State, S) ->
	?MODULE:State(Msg, S).
