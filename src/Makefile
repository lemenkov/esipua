erl_FILES = make.erl yate_demo_call.erl yate_srv.erl yate_demo.erl	\
yate_sup.erl yate_conn.erl yate_encode.erl yate_test.erl		\
yate_conn_srv.erl yate.erl yate_util.erl yate_decode.erl		\
yate_reg_srv.erl

beam_FILES = $(erl_FILES:.erl=.beam)

all: $(beam_FILES)

%.beam: %.erl
	erlc -I ../include $<
