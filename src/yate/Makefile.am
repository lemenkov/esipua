include $(top_srcdir)/rules.mk

bin_PROGRAMS = wrapper

wrapper_SOURCES = wrapper.c

OPT_APP = yate

yate_VSN = 1.0

yate_SRCS = \
command.erl \
key_dict.erl \
key_list.erl \
yate_app.erl \
yate_call.erl \
yate_conn.erl \
yate_conn_srv.erl \
yate_decode.erl \
yate_encode.erl \
yate.erl \
yate_notify.erl \
yate_pending.erl \
yate_port_srv.erl \
yate_port_sup.erl \
yate_reg_srv.erl \
yate_srv.erl \
yate_test.erl \
yate_util.erl \
yate_srv.hrl

yate_HDRS = yate.hrl

AM_ERLCFLAGS = -I$(top_srcdir)/include

echo:
	echo $(beam_FILES) $($(OPT_APP)_VSN)

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot yate -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"'
