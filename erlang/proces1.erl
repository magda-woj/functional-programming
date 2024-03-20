-module (proces1).
-export ([start/0,hello/2]).
hello(Msg,1) -> io:format("~p~n",[Msg]);
hello(Msg,N) -> io:format("~p~n",[Msg]), hello(Msg,N-1).

start() ->  spawn(proces1,hello,["Proces 1",5]),
            spawn(proces1,hello,["Proces 2",5]),
            "".
        % io:format( "PID (Proces 1) = ~p,~n PID (Proces 2) = ~p~n", [PID1,PID2]).
