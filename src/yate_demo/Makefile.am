erlangdir = $(libdir)/erlang
bindir = $(erlangdir)/yate-0.1/bin
ebindir = $(erlangdir)/yate-0.1/ebin

erl_FILES = \
yate_demo_app.erl \
yate_demo_call.erl \
yate_demo.erl \
yate_demo_sup.erl

beam_FILES = $(erl_FILES:.erl=.beam)

ebin_DATA = $(beam_FILES) yate_demo.app yate_demo.boot
EXTRA_DIST = $(erl_FILES) yate_demo.app-in yate_demo.rel
CLEANFILES = $(beam_FILES) yate_demo.app yate_demo.boot yate_demo.script

ERLFLAGS = 
EFLAGS = $(ERLFLAGS) -I$(top_srcdir)/include -pz ../yate

%.beam: %.erl
	$(ERLC) $(EFLAGS) -I $(top_srcdir)/include $<

%.app: %.app-in
	cp $< $@

%.boot: %.rel %.app
	$(ERLC) $(EFLAGS) $<
