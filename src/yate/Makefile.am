include $(top_srcdir)/application.mk

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
yate_call_reg.erl \
yate_conn.erl \
yate_conn_srv.erl \
yate_decode.erl \
yate_encode.erl \
yate.erl \
yate_notify.erl \
yate_pending.erl \
yate_port_srv.erl \
yate_port_sup.erl \
yate_srv.erl \
yate_test.erl \
yate_util.erl \
yate_srv.hrl

yate_HDRS = yate.hrl

AM_ERLCFLAGS = -I$(top_srcdir)/include

EXTRA_DIST += yate_generated.hrl.in
CLEANFILES += yate_generated.hrl

YATE_SUBST = sed \
	-e 's,[@]YATE[@],$(YATE),g'

%.hrl: %.hrl.in
	$(YATE_SUBST) $< > $@

yate_port_srv.beam: yate_generated.hrl

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot yate -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -s appmon start
