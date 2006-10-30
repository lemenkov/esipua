libsubdir = $(ERLANG_INSTALL_LIB_DIR)/$(OPT_APP)-$($(OPT_APP)_VSN)
bindir = $(libsubdir)/bin
ebindir = $(libsubdir)/ebin

beam_FILES = $($(OPT_APP)_SRCS:.erl=.beam)

ebin_DATA = $(beam_FILES) $(OPT_APP:=.app) $(OPT_APP:=.boot)
EXTRA_DIST = $($(OPT_APP)_SRCS) $(OPT_APP:=.app-in) $(OPT_APP:=.rel)
CLEANFILES = $(beam_FILES) $(OPT_APP:=.app) $(OPT_APP:=.boot)	\
$(OPT_APP:=.script)

%.beam: %.erl
	$(ERLC) $(AM_ERLFLAGS) $(ERLFLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<

%.app: %.app-in
	cp $< $@

%.boot: %.rel %.app
	$(ERLC) $(AM_ERLFLAGS) $(ERLFLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<
