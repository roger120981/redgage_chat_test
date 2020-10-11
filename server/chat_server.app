{application, chat_server,
 [{description, "Erlang Chat Server Test"},
  {vsn, "0.1.0"},
  {registered, [chat_server_gs]},
  {mod, {chat_server, []}},
  {applications, [kernel, stdlib ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.