include $(top_srcdir)/rules.mk

OPT_APP = sipb2bua

sipb2bua_VSN = 1.0

sipb2bua_SRCS = \
sipb2bua.erl

YXABUILDDIR = /home/mikael/tmp/cvs/yxa/build_linux
AM_ERL_FLAGS = \
-pz ../esipua \
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

AM_ERLCFLAGS = -I$(top_srcdir)/include -I../esipua -I/usr/lib/yxa/include

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot sipb2bua -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipb2bua.config -s appmon start
