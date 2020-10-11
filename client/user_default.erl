-module(user_default).

-export([
  login/1,
  logout/0,
  users/0,
  chat_with/1,
  send/2,
  sendg/2,
  create_group/1,
  add_to_group/2,
  remove_from_group/2,
  help/0]).

login(User) ->
  chat_client:login(User).

logout() ->
  chat_client:logout().

users() ->
  chat_client:users().

chat_with(TargetUser) ->
  chat_client:chat_with(TargetUser).

send(ChatId, Message) ->
  chat_client:send(ChatId, Message).

sendg(GroupName, Message) ->
  chat_client:sendg(GroupName, Message).

create_group(GroupName) ->
  chat_client:create_group(GroupName).

add_to_group(GroupName, User) ->
  chat_client:add_to_group(GroupName, User).

remove_from_group(GroupName, User) ->
  chat_client:remove_from_group(GroupName, User).

help() ->
  io:format(
    "> Available commands:~n"
    "-----------------------------~n"
    "- 'login(<User>).': Logs in a user to the server.~n~n"

    "- 'chat_with(<TargeUser>).': Sends a chat request to a target user. "
    "Returns a chat id that both users can use to chat with each other.~n~n"

    "- 'send(<ChatId>, <Message>).': Sends a message to the other user "
    "involved in the chat request created by 'chat_with'.~n~n"

    "- 'sendg(, ).': Sends a message all users in the group.~n~n"

    "- 'create_group(<GroupName>).': Creates a chat group to send messages "
    "to many users at the same time. See 'add_to_group', 'sendg',~n~n"
    
    "- 'add_to_group(<GroupName>, <User>).': Adds a user to a chat group.~n~n"

    "- 'remove_from_group(<GroupName>, <User>).': Removes a user from a "
    "chat group.~n~n"

    "- 'logout().': Logs out the current user from the server.~n~n"
  
    "- 'help().': Shows this help.~n").