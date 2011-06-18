%% @doc Module which handles operations with the users
-module(user_server).
-behaviour(gen_server).
-include("../include/game.hrl").

%% API
-export([start_link/1, create_new/2, login/2, clear_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TableName, ?MODULE).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Options = #user_server_options{}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

create_new(Login, Pwd) ->
    gen_server:call(?SERVER, {create_new, Login, Pwd}).

login(Login, Pwd) ->
    gen_server:call(?SERVER, {login, Login, Pwd}).

clear_all() ->
    gen_server:call(?SERVER, {clear_all}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initializes user_server. It loads dets table of users list.
-spec init(#user_server_options{}) -> {ok, State::#state{}} | {stop, Reason::term()}.
init(ServerOptions = #user_server_options{dataPath = DataPath}) ->
    Opts = [
	    {file, DataPath},
	    {keypos, 2}
	   ],
    ?Log("Started in directory: ~p", DataPath),
    case dets:open_file(?TableName, Opts) of
	{ok, ?TableName} -> {ok, #state{}};
	{error, _Reason} -> {stop, _Reason}
    end.

%% Handles login messages
handle_call({login, Login, Pwd}, _, State) ->
    Reply = process_login(Login, Pwd),
   {reply, Reply, State};

%% Handles creation of the new user
handle_call({create_new, Login, Pwd}, _, State) ->
    Reply = create_new_user(Login, Pwd),
    {reply, Reply, State};

handle_call({clear_all}, _, State) ->
    Reply = process_clear_all(),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Creates the new user
-spec create_new_user(string(), string()) -> {ok, registered} | {error, already_registered}.
create_new_user(Login, Pwd) ->
    case dets:lookup(?TableName, Login) of
	[] -> dets:insert(?TableName, #user{login=Login, password=Pwd, regionId=1}),
	      {ok, registered};
	_ -> {error, already_registered}
    end.

%% @doc Returns an user with the specified login and password from the database.
-spec process_login(string(), string()) -> {ok, #user{}} | {error, Reason :: term()}.
process_login(Login, Pwd) ->
    case dets:lookup(?TableName, Login) of
	[User] -> case User#user.password of
		      Pwd -> {ok, User};
		      _ -> {error, wrong_password}
		  end;
	_ -> {error, not_registered}
    end.

%% @doc Clear all the users
-spec clear_all()-> ok | {error, Reason :: term()}.
process_clear_all() ->
    dets:delete_all_objects(?TableName).
