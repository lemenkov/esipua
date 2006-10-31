include $(top_srcdir)/rules.mk

OPT_APP = sipb2bua

sipb2bua_VSN = 1.0

sipb2bua_SRCS = \
sipb2bua.erl

AM_ERL_FLAGS = -pz ../esipua
AM_ERLCFLAGS = -I../esipua

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot sipb2bua -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipb2bua.config -s appmon start
