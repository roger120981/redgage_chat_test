-module(chat_client_gs).
-behavior(gen_server).

%% API
-export([
  connect/1,
  login/0,
  logout/0,
  users/0,
  chat_with/1,
  send/2,
  sendg/2,
  create_group/1,
  add_to_group/2,
  remove_from_group/2,
  start_link/1]).

%% Supervisor callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2]).

-record(state, {
  user::atom()
}).

-define(SERVER, chat_server_gs).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Connects to the server using app config parameters.
%% @end
%%--------------------------------------------------------------------
-spec connect(User) -> 
    ok
  | {error, Reason}
 when
    User::atom(),
    Reason::term().

connect(User) when is_atom(User) ->
  gen_server:call(?MODULE, {connect, User}, 10000).

%%--------------------------------------------------------------------
%% @doc
%% Logs in the user to the server.
%% @end
%%--------------------------------------------------------------------
-spec login() -> 
    ok
  | {error, Reason}
 when
    Reason::term().
 
login() ->
  gen_server:call(?MODULE, login).

%%--------------------------------------------------------------------
%% @doc
%% Logs out the user from the server.
%% @end
%%--------------------------------------------------------------------
-spec logout() ->
    ok
  | {error, Reason}
 when
    Reason::term().

logout() ->
  gen_server:call(?MODULE, logout).

%%--------------------------------------------------------------------
%% @doc
%% Lists out the users in the server.
%% @end
%%--------------------------------------------------------------------
-spec users() ->
    [User, ...]
  | []
 when
    User::term().

users() ->
  gen_server:call(?MODULE, users).

%%--------------------------------------------------------------------
%% @doc
%% Creates a chat with TargetUser and returns its reference.
%% @end
%%--------------------------------------------------------------------
-spec chat_with(TargetUser) ->
    {ok, {chat_id, ChatId}}
  | {error, Reason}
 when
    TargetUser::atom(),
    ChatId::atom(),
    Reason::term().

chat_with(TargetUser) when is_atom(TargetUser) ->
  gen_server:call(?MODULE, {chat_with, TargetUser}).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to a user.
%% @end
%%--------------------------------------------------------------------
-spec send(ChatId, Message) ->
    {ok, {message_sent_to, User}}
  | {error, Reason}
 when
    ChatId::atom(),
    Message::string(),
    User::atom(),
    Reason::term().

send(ChatId, Message) when is_atom(ChatId), is_list(Message) ->
  gen_server:call(?MODULE, {send_msg, ChatId, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to a group of users.
%% @end
%%--------------------------------------------------------------------
-spec sendg(GroupName, Message) ->
    ok
  | {error, Reason}
 when
    GroupName::atom(),
    Message::string(),
    Reason::term().

sendg(GroupName, Message) when is_atom(GroupName), is_list(Message) ->
  gen_server:call(?MODULE, {send_group_msg, GroupName, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a chat group in the server.
%% @end
%%--------------------------------------------------------------------
-spec create_group(GroupName) ->
    ok
  |  {ok, {group_name, GroupName}}
  | {error, Reason}
 when
    GroupName::atom(),
    Reason::term().

create_group(GroupName) when is_atom(GroupName) ->
  gen_server:call(?MODULE, {create_group, GroupName}).

%%--------------------------------------------------------------------
%% @doc
%% Adds a user to a chat group.
%% @end
%%--------------------------------------------------------------------
-spec add_to_group(GroupName, User) ->
    ok
  | {ok, added}
  | {error, Reason}
 when
    GroupName::atom(),
    User::atom(),
    Reason::term().

add_to_group(GroupName, User) when is_atom(GroupName), is_atom(User) ->
  gen_server:call(?MODULE, {add_to_group, GroupName, User}).

%%--------------------------------------------------------------------
%% @doc
%% Removes a user from a chat group.
%% @end
%%--------------------------------------------------------------------
-spec remove_from_group(GroupName, User) ->
    ok
  | {ok, removed}
  | {error, Reason}
 when
    GroupName::atom(),
    User::atom(),
    Reason::term().

remove_from_group(GroupName, User) when is_atom(GroupName), is_atom(User) ->
  gen_server:call(?MODULE, {remove_from_group, GroupName, User}).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%====================================================================
%% Gen-Server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Init.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) ->
    {ok, State}
  | {ok, State, Timeout}
  | {ok, State, hibernate}
  | {stop, Reason}
  | ignore
 when
    Args::term(),
    State::term(),
    Timeout::timeout(),
    Reason::term().

init(_Args) ->
  io:format("~n> Chat client started at: ~p.~n~n", [self()]),
  io:format("You can use 'login(<User>).' to login to chat the server.~n"),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) ->
    {reply, Reply, NewState} 
  | {reply, Reply, NewState, Timeout}
  | {reply, Reply, NewState, hibernate}
  | {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason, Reply, NewState}
  | {stop, Reason, NewState}
 when
    Request::term(),
    Reply::term(),
    From::{pid(), Tag::term()},
    State::#state{},
    NewState::#state{},
    Timeout::timeout() | infinity,
    Reason::term().

%% @private
%% Connects to the server.
handle_call({connect, User}, _From, State) ->
  % gets config parameters.
  {ok, ServerName} = application:get_env(chat_client, server_name),
  {ok, NameType} = application:get_env(chat_client, server_name_type),
  {ok, Cookie} = application:get_env(chat_client, server_cookie),

  ClientAddress = case NameType of
    shortnames ->
      {ok, LocalHost} = inet:gethostname(),
      LocalHost;
    longnames ->
      local_ip_v4()
  end,
  
  ClientNodeName = list_to_atom(atom_to_list(User) ++ "@" ++ ClientAddress),

  {ok, _} = net_kernel:start([ClientNodeName, NameType]),
  true = erlang:set_cookie(node(), Cookie),
  
  case net_kernel:connect_node(ServerName) of
    true ->
      NewState = State#state{user = User},
      {reply, ok, NewState};
    _ ->
      net_kernel:stop(),
      {reply, {error, no_connection}, State}
  end;

%% @private
%% Logs into the server.
handle_call(login, _From, State) ->
  User = State#state.user,
  R = gen_server:call({global, ?SERVER}, {login, User}),
  erlang:monitor(process, global:whereis_name(?SERVER)),
  {reply, R, State};

%% @private
%% Logouts from the server.
handle_call(logout, _From, State) ->
  R = net_kernel:stop(),
  {reply, R, State};

%% @private
%% Users in the server.
handle_call(users, _From, State) ->
  R = gen_server:call({global, ?SERVER}, list_users),
  {reply, R, State};

%% @private
%% Chats with a user.
handle_call({chat_with, TargetUser}, _From, State) ->
  User = State#state.user,
  R = case User == TargetUser of
    true ->
      {error, no_chat_circle};
    false ->
      gen_server:call({global, ?SERVER}, {create_chat, User, TargetUser})
  end,
  {reply, R, State};

%% @private
%% Sends a message to a user.
handle_call({send_msg, ChatId, Message}, _From, State) ->
  FromUser = State#state.user,
  R = gen_server:call({global, ?SERVER},
                      {send_msg, FromUser, ChatId, Message}),
  {reply, R, State};

%% @private
%% Sends a message to a group of users.
handle_call({send_group_msg, GroupName, Message}, _From, State) ->
  FromUser = State#state.user,
  R = gen_server:call({global, ?SERVER},
                      {send_group_msg, GroupName, FromUser, Message}),
  {reply, R, State};

%% @private
%% Creates a chat group.
handle_call({create_group, GroupName}, _From, State) ->
  FromUser = State#state.user,
  R = gen_server:call({global, ?SERVER}, {create_group, GroupName, FromUser}),
  {reply, R, State};

%% @private
%% Adds a user to a chat group.
handle_call({add_to_group, GroupName, User}, _From, State) ->
  R = gen_server:call({global, ?SERVER}, {add_to_group, GroupName, User}),
  {reply, R, State};

%% @private
%% Removes a user from a chat group.
handle_call({remove_from_group, GroupName, User}, _From, State) ->
  R = gen_server:call({global, ?SERVER}, {remove_from_group, GroupName, User}),
  {reply, R, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request, State) ->
    {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason, NewState}
 when
    Request::term(),
    State::#state{},
    NewState::#state{},
    Timeout::timeout(),
    Reason::term().

handle_cast(_Request, State) ->
  NewState = State,
  {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles info messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, State) ->
    {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason, NewState}
 when
    Info::timeout | term(),
    State::term(),
    NewState::term(),
    Timeout::timeout(),
    Reason::normal | term().

handle_info({chat_request, FromUser, ChatId}, State) ->
  io:format(
    "~n>> '~p' wants to chat with you.~n"
    "You can now use 'send(~p, <Message>).' to send a message to '~p'.~n",
    [FromUser, ChatId, FromUser]),
  {noreply, State};

handle_info({user_disconnected, User}, State) ->
  io:format("~n> '~p' got disconnected!~n", [User]),
  {noreply, State};

handle_info({message, FromUser, Message}, State) ->
  io:format(
    "~n>> '~p' sent you a message:~n"
    "~s~n", [FromUser, Message]),
  {noreply, State};

handle_info({group_message, GroupName, FromUser, Message}, State) ->
  io:format(
    "~n>> '~p' group message sent from '~p':~n"
    "~s~n", [GroupName, FromUser, Message]),
  {noreply, State};

handle_info({'DOWN', _, process, _ServerPid, noconnection}, State) ->
  io:format("~n> Disconnected from the server!~n"),
  net_kernel:stop(),
  {noreply, State};

handle_info(Info, State) ->
  io:format("~p", [Info]),
  NewState = State,
  {noreply, NewState}.

%%====================================================================
%% Private functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
local_ip_v4() ->
  {ok, Addrs} = inet:getif(),
  case element(1, hd(Addrs)) of
    IP = {_,_,_,_} ->
      inet_parse:ntoa(IP);
    _ -> "127.0.0.1"
  end.