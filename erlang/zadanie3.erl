-module (zadanie3).
-export([start/3, root/3, child/4, createChildren/5, receiveFromChildren/1]).

createChildren(PID, X, N, M, countChildren) ->
    if  ((M - countChildren) < 1) ->
            spawn(zadanie3, child, [M*X + M - 1, N-1, PID]);
        true ->
            spawn(zadanie3, child, [M*X + countChildren - 1, N-1, PID]),
            createChildren(PID, X, N, M, countChildren+1)
    end.
    % spawnujemy M dzieci, każde ma PID jako ParentID, odpowiednia wartosc i glebokosc o jeden mniejsza

receiveFromChildren(1) ->
    receive List -> 
        List
    end;
receiveFromChildren(N) ->
    receive List -> 
        List ++ receiveFromChildren(N-1)
    end.
% dostajemy wiadomosc od spodziewanej liczby dzieci (jak spodziewamy sie juz tylko jednej to odbieramy jedna wiadomosc, a jak wiecej to dodajemy do wiadomosci i odbieramy dalej

root(X, N, M) ->
    createChildren(self(), X, N, M, 1),
    % tworzymt M dzieci
    Msg = receiveFromChildren(M),
    % odbiermay od M dzieci
    io:format("~p ~n", [[X] ++ Msg]).
% wypisujemy


child(X, 0, ParentID, M) -> 
                io:format("~p ~n", [[X]]),
                ParentID ! [X];
            % jestesmy w ostatnim pokoleniu (drugi arg = 0) wiec nie tworzymy dzieci tylko wypisujemy i wysylamy do rodzica
child(X, N, ParentID, M) ->
    createChildren(self(), X, N, M, 1),
    Msg = receiveFromChildren(M),
    io:format("~p ~n", [[X] ++ Msg]),
    ParentID ! Msg.
% nie jestesmy w ostatnim pokolowniu, wiec tworzymy dzieci, odbieramy wiadomosc od dzieci i wysylamy ja do rodzica

start(M, N, X) -> spawn(zadanie3, root, [X, N, M]).