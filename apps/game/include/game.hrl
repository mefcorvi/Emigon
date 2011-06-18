%% @doc Role of the user in the system
-record(user_role, {
	  level = user
	 }).

%% @doc Definition of the user
-record(user, {
	  login :: nonempty_string(),
	  password :: nonempty_string(),
	  role = #user_role{} :: #user_role{},
	  regionId :: integer()
	 }).

-record(user_server_options, {
	  dataPath :: string()
	 }).

-define(Log(Str), log4erl:log(info, lists:concat(["[", ?MODULE, "] ", Str]))).
-define(Log(Str, Args), log4erl:log(info, lists:concat(["[", ?MODULE, "] ", Str]), [Args])).

-define(BasePath, "/home/mefcorvi/projects/game").
-define(DataPath, "/home/mefcorvi/projects/game/data").
-define(StaticPath, "/home/mefcorvi/projects/game/static").
-define(PrivatePath, "/home/mefcorvi/projects/game/private").
