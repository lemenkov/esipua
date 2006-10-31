include $(top_srcdir)/rules.mk

OPT_APP = esipua

esipua_VSN = 1.0

esipua_SRCS = \
esipua_app.erl \
esipua_sup.erl \
callregister.erl \
register_server.erl \
register_sup.erl \
sdp.erl \
sipcall_bye.erl \
sipcall.erl \
siphelper.erl \
sipregister.erl \
siptest.erl

esipua_HDRS = \
sdp.hrl \
sipclient.hrl

AM_ERLCFLAGS = -I$(top_srcdir)/include

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot esipua -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -s appmon start
