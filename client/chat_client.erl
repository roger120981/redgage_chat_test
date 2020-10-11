-module(chat_client).
-behaviour(application).

-export([start/0]).

%% API
-export([
  login/1,
  logout/0,
  users/0,
  chat_with/1,
  send/2,
  sendg/2,
  create_group/1,
  add_to_group/2,
  remove_from_group/2
]).

%% Application callbacks
-export([
  start/2,
  stop/1]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  ok = application:start(chat_client).

%%--------------------------------------------------------------------
%% @doc
%% Connects and logins the user to the server.
%% @end
%%--------------------------------------------------------------------
-spec login(User) -> 
    ok
  | {error, Reason}
 when
    User::atom() | string(),
    Reason::term().

login(User) when is_list(User) ->
  login(list_to_atom(User));
login(User) when is_atom(User) ->
  net_kernel:stop(),
  case chat_client_gs:connect(User) of
    ok ->
      io:format("~n> Connected."), timer:sleep(1000),
      io:format("~n> Logging in...~n~n"), timer:sleep(1000),
      R = chat_client_gs:login(),
      io:format(
        "You can use 'users().', 'chat_with(<TargetUser>).' or even "
        "'create_group(<GroupName>).' to start chating.~n"
        "To see more available functions type 'help().'~n"),
      R;
    {error, no_connection} = Error ->
      net_kernel:stop(),
      io:format(
        "*** ERROR: Couldn't connect to the server. Please check 'server_name' "
        "configuration!~n"),
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Disconnects the user from the server.
%% @end
%%--------------------------------------------------------------------
-spec logout() ->
    ok
  | {error, Reason}
 when
    Reason::term().

logout() ->
  chat_client_gs:logout().

%%--------------------------------------------------------------------
%% @doc
%% Lists out the users in the server.
%% @end
%%--------------------------------------------------------------------
-spec users() ->
    [User, ...]
  | []
 when
    User::atom().

users() ->
  chat_client_gs:users().

%%--------------------------------------------------------------------
%% @doc
%% Creates a chat instance with a target user.
%% @end
%%--------------------------------------------------------------------
-spec chat_with(TargetUser) ->
    {ok, {chat_id, ChatId}}
  | {error, Reason}
 when
    TargetUser::atom() | string(),
    ChatId::atom(),
    Reason::term().

chat_with(TargetUser) when is_list(TargetUser) ->
  chat_with(list_to_atom(TargetUser));
chat_with(TargetUser) when is_atom(TargetUser) ->
  case chat_client_gs:chat_with(TargetUser) of
    {ok, {chat_id, ChatId}} = Chat->
      io:format("> Request sent.~n"),
      io:format("You can now use 'send(~p, <Message>).' to send a message to '~p'.~n",
        [ChatId, TargetUser]),
      Chat;
    {error, no_chat_circle} = Error ->
      io:format(
        "*** ERROR: You cannot create a chat with your self!~n"),
      Error;
    {error, user_not_connected} = Error ->
      io:format(
        "*** ERROR: The user '~p' is not connected!~n", [TargetUser]),
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to a user.
%% @end
%%--------------------------------------------------------------------
-spec send(ChatId, Message) ->
    ok
  | {error, Reason}
 when
    ChatId::atom(),
    Message::string(),
    Reason::term().

send(ChatId, Message) when is_atom(ChatId), is_list(Message) ->
  case chat_client_gs:send(ChatId, Message) of
    {ok, {message_sent_to, User}} ->
      io:format("> The message was sent to '~p'.~n", [User]),
      ok;
    {error, no_chat} = Error->
      io:format(
        "*** ERROR: Chat '~p' does not exists. Please create one using "
        "'chat_with(<TargetUser>).'!~n", [ChatId]),
      Error
  end.

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
  case chat_client_gs:sendg(GroupName, Message) of
    ok ->
      io:format("> Message sent to all users in the group '~p'.~n", [GroupName]),
      ok;
    {error, no_group} = Error->
      io:format(
        "*** ERROR: Group '~p' does not exists. Please create one using "
        "'create_group(<GroupName>).'!~n", [GroupName]),
      Error;
    {error, user_not_in_group} = Error->
      io:format(
        "*** ERROR: You are not in the group '~p'!~n", [GroupName]),
      Error;
    {error, empty_group} = Error->
      io:format(
        "*** ERROR: The group '~p' is empty. Try add some users using "
        "'add_to_group(~p, <User>).'!~n", [GroupName, GroupName]),
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a chat group.
%% @end
%%--------------------------------------------------------------------
-spec create_group(GroupName) ->
    {ok, {group_name, GroupName}}
  | {error, Reason}
 when
    GroupName::atom(),
    Reason::term().

create_group(GroupName) when is_atom(GroupName) ->
  case chat_client_gs:create_group(GroupName) of
    ok -> ok;
    {ok, {group_name, GroupName}} = Group ->
      io:format(
        "> A new chat group was created and current user added.~n"
        "You can now use 'add_to_group(~p, <User>).' and "
        "'sendg(~p, <Message>).' to send messages to all users in the group.~n",
        [GroupName, GroupName]),
      Group
  end.

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
  case chat_client_gs:add_to_group(GroupName, User) of
    ok -> ok;
    {ok, added} = OK ->
      io:format(
        "'> ~p' was added to the group '~p'.~n"
        "You can remove it at any time using 'remove_from_group(~p, ~p).'~n",
       [User, GroupName, GroupName, User]),
      OK;
    {error, no_group} = Error ->
      io:format(
        "*** ERROR: Group '~p' does not exists. Please create one using "
        "'create_group(<GroupName>).'!~n", [GroupName]),
      Error;
    {error, user_not_connected} = Error ->
      io:format(
        "*** ERROR: Cannot add the user '~p' to the group '~p' because it's "
        "not connected!~n", [User, GroupName]),
      Error
  end.

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
  case chat_client_gs:remove_from_group(GroupName, User) of
    ok -> ok;
    {ok, removed} = OK ->
      ok = io:format(
        "> '~p' was removed from the group '~p'.~n",
        [User, GroupName]),
      OK;
    {error, no_group} = Error ->
      io:format(
        "*** ERROR: Group '~p' does not exists. Please create one using "
        "'create_group(<GroupName>).'!~n", [GroupName]),
      Error
  end.
%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
  chat_client_sup:start_link().

stop(_State) ->
  ok.