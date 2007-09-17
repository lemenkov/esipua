include $(top_srcdir)/rel.mk

OPT_RELEASES = yate yate_demo sipua sipb2bua sipclient

AM_ERLCFLAGS = -pa $(top_builddir)/src/yate -pa			\
$(top_builddir)/src/yate_demo -pa $(top_builddir)/src/sipua -pa	\
$(top_builddir)/src/sipb2bua -pa $(top_builddir)/src/sipclient

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS)
# -s systools make_tar sipb2bua -s init exit
