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

-module(netstat).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(state, {port, shfile}).

init([]) ->
	MePath = filename:dirname(code:which(?MODULE)),
	ShFile = filename:join([MePath, "netstat.sh"]),
	ets:new(port_pid, [set, named_table, public, {read_concurrency, true}]),
	ets:new(pid_owner, [set, named_table, public, {read_concurrency, true}]),
	{ok, #state{shfile = ShFile}, 0}.

terminate(_Reason, _S) -> ok.

handle_call(_M, _From, S) ->
	{noreply, S}.

handle_cast(_M, S) ->
	{noreply, S}.

netstat_loop(Port) ->
	receive
		{Port, {data, {_Flag, Line}}} ->
			Parts = [P || P <- binary:split(Line, [<<" ">>, <<"\t">>], [global]), not (P =:= <<>>)],
			case Parts of
				[<<"tcp">>, _, _, Listener, _, <<"LISTEN">>, PidProg] ->
					[_, NumPart] = binary:split(Listener, <<":">>, [trim]),
					case binary:split(PidProg, <<"/">>, [trim]) of
						[Pid, Program] -> ok;
						_ -> Pid = <<"0">>, Program = <<"?">>
					end,
					Num = case (catch list_to_integer(binary_to_list(NumPart))) of
		                {'EXIT', _} -> 0;
		                N -> N
		            end,
		            if (Num > 1024) andalso (Num < 65535) ->
		            	if (Num >= 5012) andalso (Num =< 6170) andalso (((Num - 5012) rem 3) == 0) ->
		            		ok;
		            	true ->
			            	{ok, _} = server_index:get(Num),
			            	ets:insert(port_pid, {Num, list_to_integer(binary_to_list(Pid)), Program})
			            end;
		            true ->
		            	ok
		            end,
		            netstat_loop(Port);
		        _ ->
		        	netstat_loop(Port)
		    end;
		{Port, {exit_status, 0}} ->
			ok;
		{Port, {exit_status, S}} ->
			throw({badexit, S})
	after 10000 ->
		throw(timeout)
	end.

ps_loop(Port) ->
	receive
		{Port, {data, {_Flag, Line}}} ->
			Parts = [P || P <- binary:split(Line, [<<" ">>, <<"\t">>], [global]), not (P =:= <<>>)],
			case Parts of
				[<<"USER">>, <<"PID">>] ->
					ps_loop(Port);
				[Owner, Pid] ->
					PidNum = list_to_integer(binary_to_list(Pid)),
					ets:insert(pid_owner, {PidNum, Owner}),
					ps_loop(Port);
		        _ ->
		        	ps_loop(Port)
		    end;
		{Port, {exit_status, 0}} ->
			ok;
		{Port, {exit_status, S}} ->
			throw({badexit, S})
	after 10000 ->
		throw(timeout)
	end.

handle_info(timeout, S = #state{shfile = ShFile}) ->
	Port = erlang:open_port(
		{spawn_executable, ShFile},
		[binary, exit_status, {line, 1024}]),
	netstat_loop(Port),
	Port2 = erlang:open_port(
		{spawn, "ps -ao user,pid"},
		[binary, exit_status, {line, 4096}]),
	ps_loop(Port2),
	{noreply, S#state{port = Port}, 1000};

handle_info(_M, S) ->
	{noreply, S}.
