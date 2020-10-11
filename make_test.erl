-module(make_test).

-export([start/0]).

start() ->
  shell_default:cd("./client"),
  make:all(),
  shell_default:cd("../server"),
  make:all(),
  halt().
 