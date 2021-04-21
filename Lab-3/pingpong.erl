-module(pingpong).
-export([start/0, stop/0, play/1, ping_loop/1, pong_loop/1]).

-import(timer,[sleep/1]).
-import(io, [fwrite/2]).

start() ->
  register(ping, spawn(fun() -> ping_loop(0) end)),
  register(pong, spawn(fun() -> pong_loop(0) end)),
  play(15).

stop() ->
  ping ! -1,
  pong ! -1.

ping_loop(Sum) ->
  receive
    -1 -> ok;
    0 -> ping_loop(Sum);
    N -> io:format("Ping: ~w Sum ~w~n", [N, Sum]),
    timer:sleep(1000),
    pong ! (N-1),
    ping_loop(Sum + N)
after
  20000 -> ok
end.

pong_loop(Sum) ->
  receive
    -1 -> ok;
    0 -> ping_loop(Sum);
    N -> io:format("Pong: ~w Sum ~w~n", [N, Sum]),
    timer:sleep(1000),
    ping ! (N-1),
    pong_loop(Sum + N)
after
  7000 -> ok
end.

play(N) when is_integer(N) andalso N>=0 ->
  ping ! N.
