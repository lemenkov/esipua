erlangdir = $(libdir)/erlang
ebindir = $(erlangdir)/esipua-0.1/ebin

erl_FILES = \
sdp.erl \
sipclient.erl \
ysip_srv.erl

beam_FILES = $(erl_FILES:.erl=.beam)

ebin_DATA = $(beam_FILES) sipclient.app sipclient.boot
EXTRA_DIST = $(erl_FILES) sipclient.app-in sipclient.rel
CLEANFILES = $(beam_FILES) sipclient.app sipclient.boot sipclient.script

YXABUILDDIR = /home/mikael/tmp/cvs/yxa/build_linux
ERLFLAGS = \
-pz ../yate \
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

EFLAGS = $(ERLFLAGS) -I$(top_srcdir)/include -I/usr/lib/yxa/include

%.beam: %.erl
	\$(ERLC) $(EFLAGS) -I $(top_srcdir)/include $<

%.app: %.app-in
	cp $< $@

%.boot: %.rel %.app
	$(ERLC) $(EFLAGS) $<

run:
	$(ERL) $(ERLFLAGS) -name eryate -boot yate -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -yxa_config sipclient.config
#	$(ERL) -boot yate -sasl sasl_error_logger '{file,"sasl.log"}'
