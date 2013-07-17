ERLANG := erl -pa ebin -pa deps/*/ebin -smp enable -setcookie my_cookie ${ERL_ARGS}
CLASSPATH := ./bin:/usr/local/lib/erlang/lib/jinterface-1.5.6/priv/OtpErlang.jar:"./priv/*"

all: clean
	rebar get-deps && rebar --verbose compile

erl:
	rebar skip_deps=true --verbose compile

clean:
	rebar clean

build_plt: erl
	dialyzer --verbose --build_plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools \
				    compiler --output_plt .fetjaba.plt -pa deps/*/ebin ebin

analyze: erl
	dialyzer --verbose -pa deps/*/ebin --plt .fetjaba.plt -Werror_handling ebin

xref: all
	rebar skip_deps=true xref

shell: erl
	if [ -n "${NODE}" ]; then ${ERLANG} -name ${NODE}@`hostname` -boot start_sasl; \
	else ${ERLANG} -name fetjaba@`hostname` -boot start_sasl; \
	fi

run: erl
	if [ -n "${NODE}" ]; then ${ERLANG} -name ${NODE}@`hostname` -boot start_sasl -s fetjaba_app; \
	else ${ERLANG} -name fetjaba@`hostname` -boot start_sasl -s fetjaba_app; \
	fi

test: erl
	mkdir -p log/ct
	mkdir -p log/java
	java -classpath ${CLASSPATH} net.sourceforge.cobertura.instrument.Main bin
	jar cf priv/fetjaba.jar -C bin .
	rebar skip_deps=true ct -vvv
	java -classpath ${CLASSPATH} net.sourceforge.cobertura.reporting.Main --destination log/java java_src
	open log/ct/index.html
	open log/java/index.html

doc: erl
	rebar skip_deps=true doc
	javadoc -overview doc/overview-summary.html \
			-classpath ${CLASSPATH} \
			-verbose -d doc/java -use -version -author `find java_src -name *.java`