%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc From Erlang To Java And Back Again
%%% @end
%%%-------------------------------------------------------------------
-module(fetjaba_app).
-author('elbrujohalcon@inaka.net').

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

%%-------------------------------------------------------------------
%% ADMIN API
%%-------------------------------------------------------------------
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, fetjaba}}.
start() -> application:start(fetjaba).

%% @doc Stops the application
-spec stop() -> ok.
stop() -> application:stop(fetjaba).

%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) -> fetjaba:start_link().

%% @private
-spec stop(any()) -> ok.
stop(_State) -> fetjaba:stop().