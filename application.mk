libsubdir = $(ERLANG_INSTALL_LIB_DIR)/$(OPT_APP)-$($(OPT_APP)_VSN)
bindir = $(libsubdir)/bin
ebindir = $(libsubdir)/ebin
incdir = $(libsubdir)/include

inc_HEADERS = $($(OPT_APP)_HDRS)
beam_FILES = $($(OPT_APP)_SRCS:.erl=.beam)

ebin_DATA = $(beam_FILES) $(OPT_APP:=.app)
EXTRA_DIST = $($(OPT_APP)_SRCS) $(OPT_APP:=.app-in)
CLEANFILES = $(beam_FILES) $(OPT_APP:=.app)

%.beam: %.erl
	@echo [ERLC] $@
	@$(ERLC) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<

%.app: %.app-in
	cp $< $@
