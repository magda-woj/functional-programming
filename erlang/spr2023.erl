-module (spr2023).
-export([start/2, root/2, child/3]).

root(X, N) ->
    Left = spawn(spr2023, child, [2*X, N-1, self()]),
    Right = spawn(spr2023, child, [2*X+1, N-1, self()]),
    % io:format("korzen: ~p lewe: ~p prawe: ~p ~n", [self(), Left, Right]),
    MsgL = receive
       {Left, ListL} -> ListL
    end,
    MsgR = receive
        {Right, ListR} -> ListR 
    end,
    io:format("~p ~n", [[X] ++ MsgL ++ MsgR]).

child(X, 0, ParentID) -> 
    % io:format("rodzic: ~p dzieciuch: ~p ~n", [ParentID, self()]),
                io:format("~p ~n", [[X]]),
                ParentID ! {self(), [X]};
child(X, N, ParentID) ->
    Left = spawn(spr2023, child, [2*X, N-1, self()]),
    Right = spawn(spr2023, child, [2*X+1, N-1, self()]),
    % io:format("korzen: ~p lewe: ~p prawe: ~p ~n", [self(), Left, Right]),
    MsgL = receive
       {Left, ListL} -> ListL
    end,
    MsgR = receive
        {Right, ListR} -> ListR 
    end,
    Msg = [X] ++ MsgL ++ MsgR,
    io:format("~p ~n", [Msg]),
    ParentID ! {self(), Msg}.

start(X, N) -> spawn(spr2023, root, [X, N]).