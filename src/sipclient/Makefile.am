include $(top_srcdir)/app.mk

OPT_APP = sipclient

sipclient_VSN = 1.0

sipclient_SRCS = \
sipclient.erl \
ysip_srv.erl

AM_ERL_FLAGS = \
-pz ../yate \
-pz ../yate_demo \
-pz ../sipua

AM_ERLCFLAGS = -I$(srcdir)/../yate -I$(srcdir)/../sipua

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot sipclient -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipclient.config -s appmon start
#	$(ERL) -boot yate -sasl sasl_error_logger '{file,"sasl.log"}'
