.PHONY: all test doc

all:
	@[ -f private/game_log.log ] && rm private/game_log.log || echo "Log file not exists"
	rebar skip_deps=true compile
	rebar -f generate

test:
	rebar skip_deps=true eunit

doc:
	set -v off
	rebar doc
