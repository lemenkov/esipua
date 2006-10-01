erlangdir = $(libdir)/erlang
bindir = $(erlangdir)/yate-0.1/bin
ebindir = $(erlangdir)/yate-0.1/ebin

bin_PROGRAMS = wrapper

wrapper_SOURCES = wrapper.c

erl_FILES = \
command.erl \
key_dict.erl \
key_list.erl \
yate_app.erl \
yate_call.erl \
yate_clock.erl \
yate_conn.erl \
yate_conn_srv.erl \
yate_decode.erl \
yate_demo_app.erl \
yate_demo_call.erl \
yate_demo.erl \
yate_demo_sup.erl \
yate_encode.erl \
yate.erl \
yate_notify.erl \
yate_pending.erl \
yate_port_srv.erl \
yate_port_sup.erl \
yate_reg_srv.erl \
yate_srv.erl \
yate_test.erl \
yate_util.erl

beam_FILES = $(erl_FILES:.erl=.beam)

ebin_DATA = $(beam_FILES) yate.app yate.boot yate_demo.app yate_demo.boot
EXTRA_DIST = $(erl_FILES) yate.app-in yate.rel yate_demo.app-in yate_demo.rel
CLEANFILES = $(beam_FILES) yate.app yate.boot yate.script yate_demo.app yate_demo.boot yate_demo.script

ERLFLAGS = 
EFLAGS = $(ERLFLAGS) -I$(top_srcdir)/include

%.beam: %.erl
	$(ERLC) $(EFLAGS) -I $(top_srcdir)/include $<

%.app: %.app-in
	cp $< $@

%.boot: %.rel %.app
	$(ERLC) $(EFLAGS) $<
