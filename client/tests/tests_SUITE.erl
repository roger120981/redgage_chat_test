-module(tests_SUITE).
-include_lib("ct.hrl").

-import(user_default, [
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

-export([
  groups/0,
  all/0]).

-export([
  check_login/1,
  check_chat_with/1,
  check_send/1,
  check_create_group/1,
  check_add_to_group/1,
  check_remove_from_group/1,
  check_sendg/1,
  check_users/1]).

-export([login_test_other/0]).

groups() -> [
  {group_login, [sequence], [
    check_login,
    {group, group_chat_with},
    {group, group_create_chat_group},
    check_users]},
  
  {group_chat_with, [sequence], [
    check_chat_with,
    check_send]},
  
  {group_create_chat_group, [sequence], [
    check_create_group,
    check_add_to_group,
    check_sendg,
    check_remove_from_group]}].

all() -> [{group, group_login}].

%%====================================================================
%% Test cases
%%====================================================================

check_login(_Config) ->
  ok = login(test).

check_chat_with(_Config) ->
  {error, no_chat_circle} = chat_with(test),
  {error, user_not_connected} = chat_with(notest),
  {ok, {chat_id, testtest_other} = ChatHandler} = chat_with(test_other),
  {save_config, [ChatHandler]}.

check_send(Config) ->
  {check_chat_with, OldConfig} = ?config(saved_config, Config),
  ChatId = ?config(chat_id, OldConfig),
  ok = send(ChatId, "test msg!"),
  {error, no_chat} = send(no_chat_id, "test msg!").

check_create_group(_Config) ->
  {ok, {group_name, test_group} = GroupHandler} = create_group(test_group),
  ok = create_group(test_group),
  {save_config, [GroupHandler]}.

check_add_to_group(Config) ->
  {check_create_group, OldConfig} = ?config(saved_config, Config),
  GroupName = ?config(group_name, OldConfig),
  {ok, added} = add_to_group(GroupName, test_other),
  {error, no_group} = add_to_group(no_group_name, test_other),
  {error, no_group} = add_to_group(no_group_name, no_test),
  {error, user_not_connected} = add_to_group(GroupName, no_test),
  {save_config, OldConfig}.

check_sendg(Config) ->
  {check_add_to_group, OldConfig} = ?config(saved_config, Config),
  GroupName = ?config(group_name, OldConfig),
  ok = sendg(GroupName, "test group msg!"),
  {error, no_group} = sendg(no_group_name, "test group msg!"),
  {ok, removed} = remove_from_group(GroupName, test_other),
  {error, empty_group} = sendg(GroupName, "test group msg!"),
  {ok, added} = add_to_group(GroupName, test_other),
  {ok, removed} = remove_from_group(GroupName, test),
  {error, user_not_in_group} = sendg(GroupName, "test group msg!"),
  {ok, removed} = remove_from_group(GroupName, test_other),
  {error, no_group} = sendg(GroupName, "test group msg!").

check_remove_from_group(_Config) ->
  {ok, {group_name, test_group = GroupName}} = create_group(test_group),
  {ok, added} = add_to_group(GroupName, test_other),
  {ok, removed} = remove_from_group(GroupName, test_other),
  {error, no_group} = remove_from_group(no_group_name, test_other).
  
check_users(_Config) ->
  [test, test_other] = lists:sort(users()).

%%====================================================================
%% Helper functions
%%====================================================================

login_test_other() ->
    login(test_other).