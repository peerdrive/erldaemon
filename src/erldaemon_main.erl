%% This file is part of erldaemon.
%% Copyright (C) 2013  Jan Klötzke <jan AT kloetzke DOT net>
%%
%% Erldaemon is free software: you can redistribute it and/or modify it under
%% the terms of the GNU Lesser General Public License as published by the Free
%% Software Foundation, either version 3 of the License, or (at your option)
%% any later version.
%%
%% Erldaemon is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with erldaemon. If not, see <http://www.gnu.org/licenses/>.

-module(erldaemon_main).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, terminate/2]).


start_link() ->
	gen_server:start_link(?MODULE, [], []).


init([]) ->
	case os:getenv("ERLDAEMON") of
		false ->
			Port = undefined;
		Handles ->
			[InFdStr, OutFdStr] = string:tokens(Handles, ","),
			In = list_to_integer(InFdStr),
			Out = list_to_integer(OutFdStr),
			Port = open_port({fd, In, Out}, [{line, 128}]),
			port_command(Port, "detach 0\n")
	end,
	{ok, Port}.

handle_info({Port, {data, {eol, Msg}}}, Port) ->
	case Msg of
		"stop" ->
			init:stop();
		_ ->
			error_logger:error_report([{erldaemon, 'unknown request'},
				{request, Msg}])
	end,
	{noreply, Port};

handle_info(_Info, Port) ->
	{noreply, Port}.

handle_call(_Request, _From, State) -> {noreply, State}.
handle_cast(_Request, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.

