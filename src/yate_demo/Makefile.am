include $(top_srcdir)/rules.mk

OPT_APP = yate_demo

yate_demo_VSN = 1.0

yate_demo_SRCS = \
yate_clock.erl \
yate_demo_app.erl \
yate_demo_call.erl \
yate_demo.erl \
yate_demo_sup.erl

AM_ERL_FLAGS = -pz ../yate
AM_ERLCFLAGS = -I$(srcdir)/../yate

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot yate_demo -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"'
