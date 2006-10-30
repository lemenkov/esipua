include $(top_srcdir)/rules.mk

OPT_APP = sipclient
#OPT_APP = sipb2bua

sipb2bua_VSN = 1.0

sipb2bua_SRCS = \
callregister.erl \
register_server.erl \
register_sup.erl \
sdp.erl \
sipb2bua.erl \
sipcall_bye.erl \
sipcall.erl \
sipclient.erl \
siphelper.erl \
sipregister.erl \
siptest.erl \
ysip_srv.erl

sipclient_VSN = 1.0

sipclient_SRCS = \
callregister.erl \
register_server.erl \
register_sup.erl \
sdp.erl \
sipb2bua.erl \
sipcall_bye.erl \
sipcall.erl \
sipclient.erl \
siphelper.erl \
sipregister.erl \
siptest.erl \
ysip_srv.erl

YXABUILDDIR = /home/mikael/tmp/cvs/yxa/build_linux
AM_ERLFLAGS = \
-pz ../yate \
-pz ../yate_demo \
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

AM_ERLCFLAGS = -I$(top_srcdir)/include -I../yate_demo -I/usr/lib/yxa/include

run:
	$(ERL) $(AM_ERLFLAGS) $(ERLFLAGS) -name eryate -boot sipclient -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipclient.config
#	$(ERL) -boot yate -sasl sasl_error_logger '{file,"sasl.log"}'

run-b2bua:
	$(ERL) $(AM_ERLFLAGS) $(ERLFLAGS) -name eryate -boot sipb2bua -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipb2bua.config
