erl_FILES = yate_demo_call.erl yate_srv.erl yate_demo.erl	\
yate_conn.erl yate_encode.erl yate_test.erl		\
yate_conn_srv.erl yate.erl yate_util.erl yate_decode.erl		\
yate_app.erl yate_demo_sup.erl yate_demo_app.erl	\
yate_port_srv.erl yate_port_sup.erl

beam_FILES = $(erl_FILES:.erl=.beam)

ERLC = erlc
ERL = erl

all: $(beam_FILES) yate.boot yate_demo.boot

clean:
	$(RM) $(beam_FILES) yate.boot yate.script yate.app yate_demo.boot yate_demo.script yate_demo.boot

run:
	$(ERL) -boot yate_demo -sasl sasl_error_logger '{file,"sasl.log"}'
#	$(ERL) -boot yate -sasl sasl_error_logger '{file,"sasl.log"}'

%.beam: %.erl
	$(ERLC) -I ../include $<

%.app: %.app-in
	cp $< $@

yate.boot: yate.app yate.rel
	$(ERLC) -I../include yate.rel

yate_demo.boot: yate_demo.app yate_demo.rel yate.app
	$(ERLC) -I../include yate_demo.rel
