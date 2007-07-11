include $(top_srcdir)/app.mk

OPT_APP = sipua

sipua_VSN = 1.0

sipua_SRCS = \
sipua_app.erl \
sipua_sup.erl \
callregister.erl \
register_server.erl \
register_sup.erl \
sdp.erl \
sipcall_bye.erl \
sipcall.erl \
siphelper.erl \
sipregister.erl \
siptest.erl

sipua_HDRS = \
sdp.hrl \
sipclient.hrl

AM_ERLCFLAGS = -I$(top_srcdir)/include

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot sipua -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -s appmon start
