SUBST = sed \
	-e 's,[@]ERLANG_LIB_VER_kernel[@],$(ERLANG_LIB_VER_kernel),g' \
	-e 's,[@]ERLANG_LIB_VER_stdlib[@],$(ERLANG_LIB_VER_stdlib),g' \
	-e 's,[@]ERLANG_LIB_VER_sasl[@],$(ERLANG_LIB_VER_sasl),g'

%.rel: %.rel.in
	$(SUBST) $< > $@

%.beam: %.erl
	@echo [ERLC] $@
	@$(ERLC) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<

%.app: %.app-in
	$(SUBST) $< > $@

%.boot: %.rel
	@echo [ERLC] $@
	@$(ERLC) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<
