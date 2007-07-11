include $(top_srcdir)/app.mk

OPT_APP = sipb2bua

sipb2bua_VSN = 1.0

sipb2bua_SRCS = \
sipb2bua.erl

AM_ERL_FLAGS = -pz ../sipua
AM_ERLCFLAGS = -I../sipua

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot sipb2bua -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipb2bua.config -s appmon start
