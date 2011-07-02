all:
	@[ -f private/game_log.log ] && rm private/game_log.log || echo "Log file not exists"
	rebar compile
	rebar -f generate

doc:
	set -v off
	rebar doc
