all:
	rebar compile

test: all
	rebar eunit

clean:
	rebar clean

