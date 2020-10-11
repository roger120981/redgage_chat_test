-module(chat_client_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0]).

%% Supervisor callbacks
-export([
  init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Init.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) ->
    {ok, {SupFlags, [ChildSpec]}}
  | ignore
 when
    Args::term(),
    SupFlags::
       #{strategy => Strategy, intensity => non_neg_integer(), period => pos_integer()}
      | {RestartStrategy::Strategy, Intensity::non_neg_integer(), Period::pos_integer()},
    ChildSpec::
       #{id:= ChildId, start:= MFArgs, restart => Restart, shutdown => Shutdown, type => Worker, modules => Modules}
      | {ChildId, StartFunc::MFArgs, Restart, Shutdown, Type::Worker, Modules::Modules},
    Strategy::one_for_all | one_for_one | rest_for_one | simple_one_for_one,
    ChildId::term(),
    MFArgs::{M::module(), F::atom(), A::[term()] | undefined},
    Restart::permanent | transient | temporary,
    Shutdown::brutal_kill | timeout(),
    Worker::worker | supervisor,
    Modules::[module()] | dynamic.

init(Args) ->
  {ok, {#{strategy => one_for_all, intensity => 10, period => 10}, [
    #{id => chat_client_gs,
      start => {chat_client_gs, start_link, [Args]},
      restart => permanent,
      shutdown => 10000,
      type => worker,
      modules => [chat_client_gs]
    }
  ]}}.