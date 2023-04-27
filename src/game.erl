% A simple game written in erlang and using messages.

-module(game).
-export([start/1, execute/2, run/1]).

% Start a process to execute an action
start(State) ->
    spawn(?MODULE,run, [State]).

% Execute on action on a process. (Action is a Message)
% The client function.
execute(Pid, Action) ->
    Pid ! {self(), Action},
    receive
        Response ->
            Response
    end.

% Waits for a message and executes the action.
% The server function.
run(State) ->
    {_, New_State} = receive
        {Pid, list} ->
            Pid ! {received, State};
        {Pid, {place, {Value, Position}}} ->
            Pid ! {received, maps:merge(State, #{Position => Value})};
        {Pid, {state, Map}} ->
            Pid ! {received, Map};
        {Pid, {check, Map}} ->
            Win = check(Map),
            Pid ! Win,
            {Win, Map};
        {Pid, _} ->
            Pid ! {{fail, unrecognized_message}, State}
    end,
    run(New_State).


check(Map) -> false.


-ifdef(EUNIT).
% ---------------------------------------------------------------------------
% Eunit Tests
% ---------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

list_test_() ->
{setup,
    fun() -> % runs before test.
        Pid = spawn(?MODULE,run, [#{}]),
        register(list_tester, Pid)
    end,

    fun(_) -> % runs after test.
        unregister(list_tester)
    end,

    [ ?_assertEqual({received, #{}}, execute(list_tester, list))
    ]
}.


place_test_() ->
{setup,
    fun() -> % runs before test.
        Pid = spawn(?MODULE,run, [#{}]),
        register(place_tester, Pid)
    end,

    fun(_) -> % runs after test.
        unregister(place_tester)
    end,

    [ ?_assertEqual({received, #{1 => x}}, execute(place_tester, {place, {x, 1}}))
    ]
}.


state_test_() ->
{setup,
    fun() -> % runs before test.
        Pid = spawn(?MODULE,run, [#{}]),
        register(state_tester, Pid)
    end,

    fun(_) -> % runs after test.
        unregister(state_tester)
    end,

    [ ?_assertEqual({received, #{1 => x, 2 => o, 5 => x, 4 => o}},
             execute(state_tester, {state, #{1 => x, 2 => o, 5 => x, 4 => o}})),
      ?_assertEqual({received, #{4 => x, 3 => o, 5 => x, 2 => o}},
             execute(state_tester, {state, #{4 => x, 3 => o, 2 => o, 5 => x}}))
    ]
}.


check_test_() ->
{setup,
    fun() -> % runs before test.
        Pid = spawn(?MODULE,run, [#{}]),
        register(check_tester, Pid)
    end,

    fun(_) -> % runs after test.
        unregister(check_tester)
    end,

    [
      ?_assert(execute(check_tester, {check, #{1 => x, 2 => x, 3 => x}})),
      ?_assert(execute(check_tester, {check, #{4 => x, 5 => x, 6 => x}})),
      ?_assert(execute(check_tester, {check, #{7 => x, 8 => x, 9 => x}})),

      ?_assert(execute(check_tester, {check, #{1 => x, 4 => x, 7 => x}})),
      ?_assert(execute(check_tester, {check, #{2 => x, 5 => x, 8 => x}})),
      ?_assert(execute(check_tester, {check, #{3 => x, 6 => x, 9 => x}})),

      ?_assert(execute(check_tester, {check, #{1 => x, 5 => x, 9 => x}})),
      ?_assert(execute(check_tester, {check, #{3 => x, 5 => x, 7 => x}})),

      ?_assertNot(execute(check_tester, {check, #{}}))

    ]
}.


other_test_() ->
{setup,
    fun() -> % runs before test.
        Pid = spawn(?MODULE,run, [#{}]),
        register(other_tester, Pid)
    end,

    fun(_) -> % runs after test.
        unregister(other_tester)
    end,

    [ ?_assertMatch({{fail, unrecognized_message}, #{}}, execute(other_tester, {other, something}))
    ]
}.


-endif.
