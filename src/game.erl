% A simple game written in erlang and using messages.

-module(game).
-export([start/1, execute/2, run/1]).
-export([check_equal/2]).

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
        {Pid, {check, {Map, Last_Position}}} ->
            Win = check(Map, Last_Position),
            Pid ! Win,
            {Win, Map};
        {Pid, _} ->
            Pid ! {{fail, unrecognized_message}, State}
    end,
    run(New_State).


check(_, Last_Position) when not is_integer(Last_Position) ->
	{fail, not_an_int};
check(Map, _Last_Position) when map_size(Map) == 0 ->
	false;
check(Map, Last_Position) ->
	Row = (Last_Position -1) div 3,
	Col = (Last_Position - 1) rem 3,
	Case1 = check_equal(Map, [Last_Position, ((Col + 1)rem 3) + (Row * 3) + 1, ((Col + 2)rem 3) + (Row * 3) + 1]),
	Case2 = check_equal(Map, [Last_Position, (Col + ((Row + 1)rem 3) * 3) + 1, (Col + ((Row + 2)rem 3) * 3) + 1]),
	Case3 = check_equal(Map, [Last_Position, ((Col + 1)rem 3) + (((Row + 1)rem 3) * 3) + 1, ((Col + 2)rem 3) + (((Row + 2)rem 3) * 3) + 1]),
	Case4 = check_equal(Map, [Last_Position, ((Col + 2)rem 3) + (((Row + 1)rem 3) * 3) + 1, ((Col + 1)rem 3) + (((Row + 2)rem 3) * 3) + 1]),
       	Case1 or Case2 or Case3 or Case4.

check_equal(Map, [H|Positions]) ->
	Comparison = fun(Position2, {Is_Equal, Value1}) ->
		Value2 = maps:get(Position2, Map, not_found),
		{(Is_Equal) and (Value1 == Value2), Value2} end,
	{Is_Equal, _} = lists:foldl(Comparison, {true, maps:get(H, Map, not_found)}, Positions),
	Is_Equal.	


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


check_equal_test_() ->
   [
     ?_assert(check_equal(#{1=>x, 2=>x, 3=>x}, [1,2,3]))
   ].


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
      ?_assert(execute(check_tester, {check, {#{1 => x, 2 => x, 3 => x}, 1}})),
      ?_assert(execute(check_tester, {check, {#{4 => x, 5 => x, 6 => x}, 5}})),
      ?_assert(execute(check_tester, {check, {#{7 => x, 8 => x, 9 => x}, 9}})),

      ?_assert(execute(check_tester, {check, {#{1 => x, 4 => x, 7 => x}, 1}})),
      ?_assert(execute(check_tester, {check, {#{2 => x, 5 => x, 8 => x}, 5}})),
      ?_assert(execute(check_tester, {check, {#{3 => x, 6 => x, 9 => x}, 9}})),

      ?_assert(execute(check_tester, {check, {#{1 => x, 5 => x, 9 => x}, 5}})),
      ?_assert(execute(check_tester, {check, {#{3 => x, 5 => x, 7 => x}, 5}})),


      ?_assertNot(execute(check_tester, {check, {#{1 => o, 2 => x, 3 => x}, 1}})),
      ?_assertNot(execute(check_tester, {check, {#{4 => x, 5 => o, 6 => x}, 5}})),
      ?_assertNot(execute(check_tester, {check, {#{7 => x, 8 => x, 9 => o}, 9}})),

      ?_assertNot(execute(check_tester, {check, {#{1 => o, 4 => x, 7 => x}, 1}})),
      ?_assertNot(execute(check_tester, {check, {#{2 => x, 5 => o, 8 => x}, 5}})),
      ?_assertNot(execute(check_tester, {check, {#{3 => x, 6 => x, 9 => o}, 9}})),

      ?_assertNot(execute(check_tester, {check, {#{1 => o, 5 => x, 9 => x}, 5}})),
      ?_assertNot(execute(check_tester, {check, {#{3 => x, 5 => x, 7 => o}, 5}})),


      ?_assertNot(execute(check_tester, {check, {#{}, 0}})),
      ?_assertEqual({fail, not_an_int}, execute(check_tester, {check, {#{}, something}}))

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
