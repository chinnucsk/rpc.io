RUN := +Bc +K true -smp enable -pa ebin -s crypto -s inets -s ssl -s elog -s socketio

all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	rebar dialyze

update-deps:
	rebar update-deps

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref
	
run: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN} -s rpcio;\
	else\
		erl  -boot start_sasl ${RUN} -s rpcio;\
	fi

shell: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN};\
	else\
		erl  -boot start_sasl ${RUN};\
	fi

test: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run rpcio_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run rpcio_tests main;\
	fi
