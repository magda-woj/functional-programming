-module (prog1).
-export ([fib/1]).
-export([srednia/1]).
-export([posNeg/1]).
-export([split/1]).


% zad1
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

% zad2
sum([]) -> 0;
sum([A|List]) -> A + sum(List).
srednia(List) -> sum(List)/length(List).

% zad3
posNeg(List) -> {length(lists:filter(fun(X) -> X < 0 end, List)), length(lists:filter(fun(X) -> X > 0 end, List))}.

% zad4
split(Lst) -> {lists:filter(fun(X) -> is_integer(X) end, Lst),lists:filter(fun(X) -> not(is_integer(X)) end, Lst)}.

% zad5

-export ([rownanieKwadratowe/3]).
 
rownanieKwadratowe(A,B,C) ->
    Delta = delta(A,B,C),
    if
        Delta > 0 ->
        {
            (-B - math:sqrt(Delta)) / (2 * A),
            (-B + math:sqrt(Delta)) / (2 * A)
        };
    Delta == 0 ->
        -B / (2 * A);
    Delta < 0 -> brakRozwiazan
    end.
 
delta(A,B,C) -> B*B - 4 * A * C.