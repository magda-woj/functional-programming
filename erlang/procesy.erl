-module (procesy).
% -export ([start/1,hello/2]).
% -export([start/0, atomB/0, atomA/1]).
% -export ([server/0,start/0,client/2]).
% hello(Msg,1) -> io:format("Proces ~p~n",[Msg]);
% hello(Msg,N) -> io:format("Proces ~p~n",[Msg]),
% hello(Msg,N-1).
% start(1) -> spawn(procesy,hello,[1,5]);
% start(N) -> spawn(procesy,hello,[N,5]), 
% start(N-1).

 
% start() ->  io:format("tworzę B\n"), 
%             PIDB = spawn(procesy, atomB, []),
%             io:format("tworzę A\n"),
%             spawn(procesy, atomA, [PIDB]).
% atomB() -> receive
%     czesc -> io:format("B: dostalem czesc\n")
%     end.
% atomA(PID) ->
%   io:format("A: wysylam czesc\n"),
%   PID ! czesc.

% %lab11 zad3 
% recvMsg() -> receive
%                 finish-> io:format("Server stopped.~n"); 
%                 (X) -> io:format("~p~n", [X * X])
%             end,
% recvMsg().
% server() -> io:format("Server started.~n",[]), recvMsg().
% client([], SPID) -> SPID ! finish;
% client([X|R], SPID) -> SPID ! X, client(R, SPID).
 
% start() -> Server_PID = spawn(zadanie3,server,[]),
%  spawn(zadanie3,client,[[1,2,3,4], Server_PID]).


-export ([server/0,startServer/0,startClient/1,client/2,recvMsg/0]).
 
recvMsg() -> 
            receive
                koniec -> io:format("Server stopped.~n");
                (X) -> if 
                        is_number(X) -> io:format("Number: ~p~n", [X]);
                        is_list(X) -> io:format("List: ~p~n", [X]);
                        true -> io:format("Atom: ~p~n", [X])
                    end,
                    recvMsg()
            end.
server() -> io:format("Server started.~n",[]), recvMsg().
 
client(finish, SPID) -> SPID ! koniec;
client(X, SPID) -> SPID ! X.
 
startClient(X) -> Server_PID = whereis(server), spawn(procesy,client,[X, Server_PID]).
 
startServer() -> Server_PID = spawn(procesy,server,[]), register(server, Server_PID).