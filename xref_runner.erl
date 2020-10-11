-module(xref_runner).

-export([start/0]).

start() ->
  xref:start(s),
  xref:add_directory(s, "./client"),
  xref:add_directory(s, "./server"),
  R1 = xref:analyze(s, undefined_function_calls),
  R2 = xref:analyze(s, undefined_functions),
  xref:stop(s),
  
  io:format("~n"),
  io:format("-----------------------------------------------------~n"),
  io:format("| Undefined Function Calls Analysis                 |~n"),
  io:format("-----------------------------------------------------~n"),
  io:format("~n~p~n~n", [R1]),
  
  io:format("-----------------------------------------------------~n"),
  io:format("| Undefined Functions Analysis                      |~n"),
  io:format("-----------------------------------------------------~n"),
  io:format("~n~p~n", [R2]).
  
  
  
 