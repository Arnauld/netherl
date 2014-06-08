all: deps compile

compile:
	./rebar get-deps compile

clean:
	./rebar clean

rel:
	./rebar generate