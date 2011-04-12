ERL := erl -pa ebin +Bc +K true -smp enable -boot start_sasl

all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	rebar skip_deps=true dialyze

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

shell: all
	${ERL}