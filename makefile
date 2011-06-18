all:
	rm private/game_log.log
	rebar compile
	rebar -f generate

doc:
	rebar doc
