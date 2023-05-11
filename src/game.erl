% A simple game written in erlang and using messages.

-module(game).
-export([start/1, execute/2, run/1]).
-export([check_equal/2]).
-export([place/2, place/3, display/1]).

place(Pid, Value, Position) ->
    execute(Pid, {place, {Value, Position}}),
    display(Pid),
    execute(Pid, {check, Position}).
place(Pid, Position) ->
    {Last, _} = execute(Pid, last),
    %io:format("Last: ~p~n", [Last]),
    if
        Last == x -> 
	    %io:format("X~n"),
            execute(Pid, {place, {o, Position}});
        Last == o ->
	    %io:format("Y~n"),
	    execute(Pid, {place, {x, Position}});
	true ->
	    fail
    end,
    display(Pid),
    execute(Pid, {check, Position}).


display(Pid) ->
    execute(Pid, display).

% Start a process to execute an action
start() ->
    spawn(?MODULE,run, [#{last => o}]).
start(Name) ->
    register(Name, start()).

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
        {Pid, {place, {Value, Position}}} when (Value == x) or (Value == 1) ->
            Pid ! {received, maps:merge(State, #{Position => $x, last => Value})};
        {Pid, {place, {Value, Position}}} when (Value == o) or (Value == 2) ->
            Pid ! {received, maps:merge(State, #{Position => $o, last => Value})};
        {Pid, {place, {Value, Position}}} ->
            Pid ! {received, maps:merge(State, #{Position => Value})};
        {Pid, {state, Map}} ->
            Pid ! {received, Map};
        {Pid, {check, Last_Position}} when (Last_Position > 0) and (Last_Position =< 9) ->
            Win = check(State, Last_Position),
            Pid ! {Win, maps:get(Last_Position, State)},
            {Win, State};
        {Pid, {check, Last_Position}} ->
	    Pid ! {{fail, Last_Position, out_of_range}, State};
        {Pid, last} ->
	    Pid ! {maps:get(last, State), State};
	{Pid, display} ->
	    E = $ ,
	    io:format("+-+-+-+~t +-+-+-+~n"),
	    %io:format("|~p|~p|~p|~n", [maps:get(1, State, 1), maps:get(2, State, 2), maps:get(3, State, 3)]),
	    io:format("|1|2|3|~t |~c|~c|~c|~n", [maps:get(1, State, E), maps:get(2, State, E), maps:get(3, State, E)]),
	    io:format("+-+-+-+~t +-+-+-+~n"),
	    %io:format("|~p|~p|~p|~n", [maps:get(4, State, 4), maps:get(5, State, 5), maps:get(6, State, 6)]),
	    io:format("|4|5|6|~t |~c|~c|~c|~n", [maps:get(4, State, E), maps:get(5, State, E), maps:get(6, State, E)]),
	    io:format("+-+-+-+~t +-+-+-+~n"),
	    %io:format("|~p|~p|~p|~n", [maps:get(7, State, 7), maps:get(8, State, 8), maps:get(9, State, 9)]),
	    io:format("|7|8|9|~t |~c|~c|~c|~n", [maps:get(7, State, E), maps:get(8, State, E), maps:get(9, State, E)]),
	    io:format("+-+-+-+~t +-+-+-+~n"),
	    %io:format("~n"),
	    %io:format("+-+-+-+~n"),
	    %io:format("|~c|~c|~c|~n", [maps:get(1, State, E), maps:get(2, State, E), maps:get(3, State, E)]),
	    %io:format("+-+-+-+\n"),
	    %io:format("|~c|~c|~c|~n", [maps:get(4, State, E), maps:get(5, State, E), maps:get(6, State, E)]),
	    %io:format("+-+-+-+~n"),
	    %io:format("|~c|~c|~c|~n", [maps:get(7, State, E), maps:get(8, State, E), maps:get(9, State, E)]),
	    %io:format("+-+-+-+~n"),
	    Pid ! {received, State};
        {Pid, _} ->
            Pid ! {{fail, unrecognized_message}, State}
    end,
    run(New_State).


%check(_, Last_Position) when not is_integer(Last_Position) ->
%	{fail, not_an_int};
check(Map, _Last_Position) when map_size(Map) == 0 ->
	false;
check(Map, Last_Position) ->
	% io:format("~p~n", [Last_Position]),
	Row = (Last_Position - 1) div 3,
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
