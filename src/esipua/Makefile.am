include $(top_srcdir)/rules.mk

OPT_APP = esipua

esipua_VSN = 1.0

esipua_SRCS = \
callregister.erl \
register_server.erl \
register_sup.erl \
sdp.erl \
sipcall_bye.erl \
sipcall.erl \
siphelper.erl \
sipregister.erl \
siptest.erl

YXABUILDDIR = /home/mikael/tmp/cvs/yxa/build_linux
AM_ERLFLAGS = \
-pz $(YXABUILDDIR) \
-pz $(YXABUILDDIR)/src \
-pz $(YXABUILDDIR)/src/cpl \
-pz $(YXABUILDDIR)/src/database \
-pz $(YXABUILDDIR)/src/eldap \
-pz $(YXABUILDDIR)/src/event \
-pz $(YXABUILDDIR)/src/local \
-pz $(YXABUILDDIR)/src/mysql \
-pz $(YXABUILDDIR)/src/sipuserdb \
-pz $(YXABUILDDIR)/src/config \
-pz $(YXABUILDDIR)/src/transactionlayer \
-pz $(YXABUILDDIR)/src/include \
-pz $(YXABUILDDIR)/src/transportlayer \
-pz $(YXABUILDDIR)/src/event_handler

AM_ERLCFLAGS = -I$(top_srcdir)/include -I/usr/lib/yxa/include
# -I../yate_demo 

run:
	$(ERL) $(AM_ERLFLAGS) $(ERLFLAGS) -name eryate -boot esipua -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -s appmon start
