SUBDIRS = src

all-local:
	test -e ebin || ln -s src ebin
#	test -e priv || ln -s c_src priv

clean-local:
	-test -L ebin && $(RM) ebin
#	-test -L priv && $(RM) priv
