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

-module(client_fsm).
-behaviour(gen_fsm).

-export([start_link/1, init/1, handle_info/3, terminate/3]).
-export([accept/2, gather_data/2]).

start_link(ListenSock) ->
	gen_fsm:start_link(?MODULE, [ListenSock], []).

-record(state, {lsock, sock}).

init([ListenSock]) ->
	{ok, accept, #state{lsock = ListenSock}, 0}.

terminate(_Reason, _State, _S) -> ok.

accept(timeout, S = #state{lsock = ListenSock}) ->
	{ok, Sock} = gen_tcp:accept(ListenSock),
	% start our replacement in the pool
	client_sup:start_client_fsm(),
	{next_state, gather_data, S#state{sock = Sock}, 0}.

clean_bin(Bin) ->
	Bin1 = binary:replace(Bin, <<"\n">>, <<" ">>, [global]),
	Bin2 = binary:replace(Bin1, <<"\r">>, <<>>, [global]),
	Bin3 = binary:replace(Bin2, <<"\t">>, <<>>, [global]),
	Bin3.
short_banner(Bin) when byte_size(Bin) > 65 ->
	binary_to_list(binary:part(clean_bin(Bin), {0,65})) ++ "...";
short_banner(Bin) -> binary_to_list(clean_bin(Bin)).

format_flags(Flags) ->
	Iflgs = format_flags([
		{has_banner, <<"b">>},
		{can_give_name, <<"n">>},
		{deals_hands, <<"H">>},
		{first_bid_ok, <<"B">>},
		{second_bid_ok, <<"2">>},
		{full_bidding, <<"f">>},
		{play_cards, <<"P">>},
		{can_finish_hand, <<"h">>},
		{extra_msg, <<"x">>}
		], Flags),
	Eflgs = format_flags([
		{down, <<"d">>},
		{protocol_err, <<"e">>},
		{disconnected_after_name, <<"n">>},
		{disconnected_before_deal, <<"D">>},
		{deal_timeout, <<"t">>},
		{dealt_bad_hand, <<"H">>},
		{accepts_bad_play, <<"p">>},
		{play_test_timeout, <<"T">>},
		{bad_pregame_disconnect, <<"E">>},
		{bad_disconnect, <<"x">>},
		{longline, <<"L">>}
		], Flags),
	{Iflgs, Eflgs}.

format_flags([], _Flags) -> <<>>;
format_flags([{Flag, Short} | Rest], Flags) ->
	RestFormat = format_flags(Rest, Flags),
	case lists:member(Flag, Flags) of
		true ->
			<<Short/binary, RestFormat/binary>>;
		_ ->
			<<"-", RestFormat/binary>>
	end.

gather_data(timeout, S = #state{sock = Sock}) ->
	Servers = server_index:all(),
	ok = gen_tcp:send(Sock, <<"===\nGrueDex\n===\n">>),
	ok = gen_tcp:send(Sock, <<"Impl flags: b  --  sends a banner line\n">>),
	ok = gen_tcp:send(Sock, <<"            n  --  doesn't die when sent player+game name\n">>),
	ok = gen_tcp:send(Sock, <<"            H  --  deals hands out to players\n">>),
	ok = gen_tcp:send(Sock, <<"            B  --  prompts for first bid and accepts it\n">>),
	ok = gen_tcp:send(Sock, <<"            2  --  prompts for second bid\n">>),
	ok = gen_tcp:send(Sock, <<"            f  --  full bidding round incl. pass\n">>),
	ok = gen_tcp:send(Sock, <<"            P  --  can play a card from hand\n">>),
	ok = gen_tcp:send(Sock, <<"            h  --  can finish a hand and lead the next\n">>),
	ok = gen_tcp:send(Sock, <<"            x  --  sends extra M messages not in spec\n">>),
	ok = gen_tcp:send(Sock, <<"Err flags:  d  --  down\n">>),
	ok = gen_tcp:send(Sock, <<"            e  --  protocol errors\n">>),
	ok = gen_tcp:send(Sock, <<"            n  --  disconnected after names\n">>),
	ok = gen_tcp:send(Sock, <<"            D  --  disconnected before deal\n">>),
	ok = gen_tcp:send(Sock, <<"            t  --  timeout waiting for hands to be dealt\n">>),
	ok = gen_tcp:send(Sock, <<"            H  --  invalid hand dealt\n">>),
	ok = gen_tcp:send(Sock, <<"            p  --  accepted an invalid play\n">>),
	ok = gen_tcp:send(Sock, <<"            T  --  timed out during play test\n">>),
	ok = gen_tcp:send(Sock, <<"            E  --  does not handle pregame disconnects correctly\n">>),
	ok = gen_tcp:send(Sock, <<"            x  --  does not in-game disconnects correctly\n">>),
	ok = gen_tcp:send(Sock, <<"            L  --  sent a >64kb line\n\n">>),
	ok = gen_tcp:send(Sock, io_lib:format("~8s ~8s ~-10s  ~-10s ~-12s  ~-70s\n", ["port", "pid", "owner", "impl", "err", "banner"])),
	ok = gen_tcp:send(Sock, io_lib:format("~8s ~8s ~-10s  ~-10s ~-12s  ~-70s\n", ["===", "===", "===", "===", "===", "==="])),
	lists:foreach(fun({Port, ServFsm}) ->
		case (catch server_fsm:info(ServFsm)) of
			{ok, _, []} -> ok;
			{ok, _, [down]} -> ok;
			{ok, Banner, Flags} ->
				[{Port, Pid, Program}] = ets:lookup(port_pid, Port),
				[{Pid, Owner}] = ets:lookup(pid_owner, Pid),
				{Impl, Err} = format_flags(Flags),
				ok = gen_tcp:send(Sock, io_lib:format(
					"~8s ~8s ~-10s  ~-10s ~-12s  ~-70s\n", [
						integer_to_list(Port),
						integer_to_list(Pid),
						Owner,
						Impl, Err,
						"\"" ++ short_banner(Banner) ++ "\""]));
			_ -> ok
		end
	end, lists:sort(Servers)),
	ok = gen_tcp:close(Sock),
	{stop, normal, S}.

handle_info(Msg, State, S) ->
	?MODULE:State(Msg, S).
