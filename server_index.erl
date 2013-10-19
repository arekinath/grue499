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

-module(server_index).

-behaviour(gen_server).
-export([start_link/0, get/1, all/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Port) ->
	case ets:lookup(servers, Port) of
		[{Port, Ref}] -> {ok, Ref};
		_ -> gen_server:call(?MODULE, {create, Port})
	end.

all() ->
	[{A,B} || [A,B] <- ets:match(servers, {'$1', '$2'})].

-record(state, {ftid, btid, stid}).

init([]) ->
	Ftid = ets:new(servers, [set, named_table, public, {read_concurrency, true}]),
	Stid = ets:new(server_status, [set, named_table, public, {read_concurrency, true}]),
	Btid = ets:new(server_mrefs, [set, private]),
	{ok, #state{ftid = Ftid, btid = Btid, stid = Stid}}.

terminate(_Reason, _S) -> ok.

handle_call({create, Port}, _From, S = #state{ftid = Ftid, btid = Btid}) ->
	case ets:lookup(Ftid, Port) of
		[{Port, Ref}] -> {reply, {ok, Ref}, S};
		_ ->
			{ok, Ref} = server_sup:start_server(Port),
			MRef = monitor(process, Ref),
			true = ets:insert(Ftid, {Port, Ref}),
			true = ets:insert(Btid, {MRef, Port}),
			{reply, {ok, Ref}, S}
	end.

handle_cast(_M, S = #state{}) ->
	{noreply, S}.

handle_info({'DOWN', MRef, process, _Pid, _Reason}, S = #state{ftid = Ftid, btid = Btid}) ->
	case ets:lookup(Btid, MRef) of
		[{MRef, Port}] ->
			true = ets:delete(Btid, MRef),
			true = ets:delete(Ftid, Port),
			{noreply, S};
		_ ->
			{noreply, S}
	end;
handle_info(_M, S = #state{}) ->
	{noreply, S}.
