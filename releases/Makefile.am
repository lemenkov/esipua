include $(top_srcdir)/rules/rel.mk

OPT_RELEASES = sipua sipb2bua

AM_ERLCFLAGS = -pa $(top_builddir)/src/yate -pa			\
$(top_builddir)/src/yate_demo -pa $(top_builddir)/src/sipua -pa	\
$(top_builddir)/src/sipb2bua -pa $(top_builddir)/src/sipclient

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS)
# -s systools make_tar sipb2bua -s init exit
