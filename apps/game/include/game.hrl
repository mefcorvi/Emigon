-record(userRole, {
	  level = user
	 }).

-record(user, {
	  login :: nonempty_string(),
	  password :: nonempty_string(),
	  role = #userRole{} :: #userRole{},
	  regionId :: integer()
	 }).

-define(Log(Str), erlang:apply(error_logger, info_msg, [lists:concat(["[", ?MODULE, "] ", Str, "~n"])])).

-define(Log(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["[", ?MODULE, "] ", Str, "~n"]), Args])).

-define(BasePath, "/home/mefcorvi/projects/game").
-define(DataPath, "/home/mefcorvi/projects/game/data").
-define(StaticPath, "/home/mefcorvi/projects/game/static").
