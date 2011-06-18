all:
	rebar compile
	rebar -f generate

doc:
	rebar doc
