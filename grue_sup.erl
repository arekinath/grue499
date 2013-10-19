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

-module(grue_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
	ServerIndex = {server_index,
		{server_index, start_link, []},
		permanent, 5000, worker, [server_index]},
	ServerSup = {server_sup,
		{server_sup, start_link, []},
		permanent, infinity, supervisor, [server_sup, server_fsm]},
	Netstat = {netstat,
		{netstat, start_link, []},
		permanent, 5000, worker, [netstat]},
	ClientSup = {client_sup,
		{client_sup, start_link, [Port]},
		permanent, infinity, supervisor, [client_sup, client_fsm]},
	{ok,
		{{one_for_one, 60, 300},
		[ServerIndex, ServerSup, Netstat, ClientSup]}}.
