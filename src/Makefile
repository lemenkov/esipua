erl_FILES = make.erl yate_demo_call.erl yate_srv.erl yate_demo.erl	\
yate_sup.erl yate_conn.erl yate_encode.erl yate_test.erl		\
yate_conn_srv.erl yate.erl yate_util.erl yate_decode.erl		\
yate_reg_srv.erl yate_app.erl

beam_FILES = $(erl_FILES:.erl=.beam)

ERLC = erlc
ERL = erl

all: $(beam_FILES) yate.boot

clean:
	$(RM) $(beam_FILES) yate.boot yate.script yate.app

run:
	$(ERL) -boot yate -s yate_demo run

%.beam: %.erl
	$(ERLC) -I ../include $<

%.app: %.app-in
	cp $< $@

yate.boot: yate.app yate.rel
	$(ERLC) -I../include yate.rel
