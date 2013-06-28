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

-export([process/0, typeof/1]).

-record(state, {java_port :: port(),
                java_node :: atom()}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the name of the class to which the term is translated on Java side
-spec typeof(term()) -> binary().
typeof(Term) ->
    ?JAVA_SERVER ! {typeof, Term, self()},
    receive
        {typeof, Term, Type} -> Type
    after 5000 ->
        throw(timeout)
    end.

%% @doc Returns the pid of the java server
-spec process() -> pid().
process() ->
    ?JAVA_SERVER ! {pid, self()},
    receive
        {pid, Pid} -> Pid
    after 5000 ->
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
  lager:error("Java node is down!"),
  {stop, nodedown, State};
handle_info({Port, {data, {eol, "SEVERE: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:error("Java Error:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, "WARNING: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:warning("Java Warning:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, "INFO: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:info("Java Info:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:debug("Java Log:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {noeol, JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:info("Java Log:\t~s...", [JavaLog]),
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
-spec terminate(_, state()) -> true.
terminate(_Reason, State) -> erlang:port_close(State#state.java_port).
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
      lager:info("Couldn't find priv dir for lucene_server, using ./priv"), "./priv";
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
      _ = lager:notice("Java node started"),
      true = link(process()),
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