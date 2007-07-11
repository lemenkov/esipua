libsubdir = $(ERLANG_INSTALL_LIB_DIR)/$(OPT_APP)-$($(OPT_APP)_VSN)
bindir = $(libsubdir)/bin
ebindir = $(libsubdir)/ebin
incdir = $(libsubdir)/include

ebin_DATA = $(OPT_RELEASES:=.boot) $(OPT_RELEASES:=.rel)	\
$(OPT_RELEASES:=.script)
EXTRA_DIST = $(OPT_RELEASES:=.rel.in)
CLEANFILES = $(OPT_RELEASES:=.boot) $(OPT_RELEASES:=.rel)	\
$(OPT_RELEASES:=.script)

SUBST = sed \
	-e 's,[@]ERLANG_LIB_VER_kernel[@],$(ERLANG_LIB_VER_kernel),g' \
	-e 's,[@]ERLANG_LIB_VER_stdlib[@],$(ERLANG_LIB_VER_stdlib),g' \
	-e 's,[@]ERLANG_LIB_VER_sasl[@],$(ERLANG_LIB_VER_sasl),g'

%.rel: %.rel.in
	$(SUBST) $< > $@

%.boot: %.rel
	@echo [ERLC] $@
	@$(ERLC) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<
