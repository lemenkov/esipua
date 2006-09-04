erl_FILES = yate_demo_call.erl yate_srv.erl yate_demo.erl	\
yate_conn.erl yate_encode.erl yate_test.erl		\
yate_conn_srv.erl yate.erl yate_util.erl yate_decode.erl		\
yate_app.erl yate_demo_sup.erl yate_demo_app.erl	\
yate_port_srv.erl yate_port_sup.erl \
sipclient.erl ysip_srv.erl

beam_FILES = $(erl_FILES:.erl=.beam)

ERLC = erlc
ERL = erl

ERLFLAGS = -pz /usr/lib/yxa/ebin
EFLAGS = $(ERLFLAGS) -I../include -I/usr/lib/yxa/include

all: $(beam_FILES) yate.boot yate_demo.boot

clean:
	$(RM) $(beam_FILES) yate.boot yate.script yate.app yate_demo.boot yate_demo.script yate_demo.boot

run:
	$(ERL) $(ERLFLAGS) -name eryate -boot yate_demo -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipclient.config
#	$(ERL) -boot yate -sasl sasl_error_logger '{file,"sasl.log"}'

%.beam: %.erl
	$(ERLC) $(EFLAGS) -I ../include $<

%.app: %.app-in
	cp $< $@

yate.boot: yate.app yate.rel
	$(ERLC) $(EFLAGS) yate.rel

yate_demo.boot: yate_demo.app yate_demo.rel yate.app
	$(ERLC) $(EFLAGS) yate_demo.rel
