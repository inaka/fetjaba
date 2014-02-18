%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc From Erlagn To Java And Back Again main module
%%% @end
%%%-------------------------------------------------------------------
-module(fetjaba).
-author('elbrujohalcon@inaka.net').

-define(JAVA_SERVER, {fetjaba_server, java_node()}).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).

-export([process/0, tick/1, tick/2]).
-export([increasing_length/0, increasing_length/1, increasing_size/0, increasing_size/1, many_requests/0, many_requests/2]).

-record(state, {java_port :: port(),
                java_node :: atom()}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the pid of the java server
-spec process() -> pid().
process() ->
    ?JAVA_SERVER ! {pid, self()},
    receive
        {pid, Pid} -> Pid
    after 5000 ->
        throw(timeout)
    end.

-spec many_requests() -> [{pos_integer(), float()}].
many_requests() ->
  Data = << <<($a + I rem 26)>> ||  I <- lists:seq(1, 50000) >>,
  [{I, many_requests(Data, I)} || I <- lists:seq(10, 1000, 10)].

-spec many_requests(term(), pos_integer()) -> float().
many_requests(Data, Times) ->
  {T, _} = timer:tc(fun() -> [tick(Data) || _ <- lists:seq(1, Times)] end),
  T/1000.


-spec increasing_size() -> [{pos_integer(), float()}].
increasing_size() ->
  [{I, increasing_size(I)} || I <- lists:seq(10000, 1000000, 10000)].

-spec increasing_size(pos_integer()) -> float().
increasing_size(Length) ->
  Data = << <<($a + I rem 26)>> ||  I <- lists:seq(1, Length) >>,
  {T, _} = timer:tc(?MODULE, tick, [Data]),
  T/1000.


-spec increasing_length() -> [{pos_integer(), float()}].
increasing_length() ->
  [increasing_length(I) || I <- lists:seq(2500, 250000, 2500)].

-spec increasing_length(pos_integer()) -> {pos_integer(), float()}.
increasing_length(Length) ->
  Data = [list_to_atom([$a + I rem 26]) || I <- lists:seq(1, Length)],
  {T, _} = timer:tc(?MODULE, tick, [Data]),
  {byte_size(term_to_binary(Data)) - 7, T/1000}.


-spec tick(pos_integer() | binary(), pos_integer()) -> proplists:proplist().
tick(Length, Times) when is_integer(Length) ->
  %tick(<< <<($a + I rem 26)>> ||  I <- lists:seq(1, Length) >>, Times);
  tick([list_to_atom([$a + I rem 26]) || I <- lists:seq(1, Length)], Times);
tick(Data, Times) ->
  TS0 = ts(),
  [tick(Data) || _ <- lists:seq(1, Times)],
  ts() - TS0.

-spec tick(pos_integer() | binary()) -> proplists:proplist().
tick(Length) when is_integer(Length) ->
  %tick(<< <<($a + I rem 26)>> ||  I <- lists:seq(1, Length) >>);
  tick([list_to_atom([$a + I rem 26]) || I <- lists:seq(1, Length)]);
tick(Data) ->
  TS1 = ts(),
  ?JAVA_SERVER ! {tick, self(), Data, TS1, TS1, TS1},
  receive
    {tick, _Pid, Data, TS1, TS2, TS3} ->
      TS4 = ts(),
      [{'erl->java', TS2 - TS1}, {'in-java', TS3 - TS2}, {'java->erl', TS4 - TS3}, {total, TS4 - TS1}, {size, byte_size(term_to_binary(Data))}]
  after 30000 ->
    throw(timeout)
  end.

%% @doc Stops the java process
-spec stop() -> stop.
stop() -> ?JAVA_SERVER ! stop.

%%-------------------------------------------------------------------
%% GEN_SERVER API
%%-------------------------------------------------------------------
%% @private
-spec init([]) -> {ok, state()}.
init([]) ->
  %% First of all we need to have java installed
  case os:find_executable("java") of
    [] ->
      throw({stop, java_missing});
    Java ->
      %% We want to let the java node know who we are
      ThisNode = atom_to_list(node()),
      %% We build a fancy name for the java node
      JavaNode = java_node(),
      %% Finding the priv dir is a tricky thing, this function does it for us
      Priv = priv_dir(fetjaba),
      %% In the same way, finding an otp lib dir is also tricky, hence otp_lib
      %% We build the classpath with our priv dir (where we put all the jars we need)
      %% and OtpErlang.jar from jinterface
      Classpath = otp_lib("/OtpErlang.jar") ++ [$: | Priv ++ "/*"],
      Port =
        erlang:open_port({spawn_executable, Java},
                         [{line,1000}, stderr_to_stdout,
                          {args, ["-classpath", Classpath, %% The classpath
                                 %% The class with the main method on our Java code
                                  "net.inaka.fetjaba.Node",
                                  %% The command line arguments
                                  %% including the node names and
                                  %% the cookie (you'll see how I use them on java side)
                                  ThisNode, JavaNode,
                                  erlang:get_cookie()]}]),
      %% Then we wait for a signal from the Java side that everything is ready
      wait_for_ready(#state{java_port = Port, java_node = JavaNode})
  end.

%% @private
-spec handle_info({nodedown, atom()}, state()) -> {stop, nodedown, state()} | {noreply, state()}.
handle_info({nodedown, JavaNode}, State = #state{java_node = JavaNode}) ->
  error_logger:error_msg("Java node is down!~n"),
  {stop, nodedown, State};
handle_info({Port, {data, {eol, "SEVERE: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  error_logger:warning_msg("Java Error:\t~s~n", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, "WARNING: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  error_logger:warning_msg("Java Warning:\t~s~n", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, "INFO: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  error_logger:info_msg("Java Info:\t~s~n", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, JavaLog}}}, State = #state{java_port = Port}) ->
  io:format("Java Log:\t~s~n", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {noeol, JavaLog}}}, State = #state{java_port = Port}) ->
  error_logger:info_msg("Java Log:\t~s...~n", [JavaLog]),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
-spec handle_call(term(), _, state()) -> {noreply, state()}.
handle_call(_Call, _From, State) -> {noreply, State}.
%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
%% @private
-spec terminate(_, state()) -> ok.
terminate(_Reason, State) -> catch erlang:port_close(State#state.java_port), ok.
%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%-------------------------------------------------------------------
%% PRIVATE
%%-------------------------------------------------------------------
%% @private
priv_dir(App) ->
  case code:priv_dir(App) of
    {error, bad_name} ->
      error_logger:info_msg("Couldn't find priv dir for the application, using ./priv~n"), "./priv";
    PrivDir -> filename:absname(PrivDir)
  end.

%% @private
%% @doc returns the absolute path to the otp erlang JAR
otp_lib(Path) ->
  JPriv = priv_dir(jinterface),
  test_priv_path(Path, file:read_file_info(JPriv ++ Path), JPriv ++ Path).

test_priv_path(_, {ok, _}, Absolute_Path) -> Absolute_Path;
test_priv_path(Path, {error, _}, _) -> filename:absname(code:lib_dir() ++ Path).

wait_for_ready(State = #state{java_port = Port}) ->
  receive
    {Port, {data, {eol, "READY"}}} ->
      error_logger:info_msg("Java node started~n"),
      Process = process(),
      true = link(Process),
      error_logger:info_msg("Process ~p linked~n", [Process]),
      true = erlang:monitor_node(State#state.java_node, true),
      {ok, State};
    Info ->
      case handle_info(Info, State) of
        {noreply, NewState} ->
          wait_for_ready(NewState);
        {stop, Reason, _NewState} ->
          {stop, Reason}
      end
  end.

java_node() ->
    case string:tokens(atom_to_list(node()), "@") of
      [Name, Server] -> list_to_atom(Name ++ "_java@" ++ Server);
      _Node -> throw({bad_node_name, node()})
    end.

ts() ->
  {X, Y, Z} = os:timestamp(),
  X * 1000000000 + Y * 1000 + trunc(Z / 1000).
