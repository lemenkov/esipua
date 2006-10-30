#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(eryate, 0.1, [Mikael Magnusson <mikma@users.sourceforge.net>])
AC_CONFIG_SRCDIR([configure.ac])

dnl AC_CANONICAL_HOST
dnl AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE

dnl AM_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_AWK
dnl AC_PROG_INSTALL
AC_PROG_LN_S
AC_ERLANG_NEED_ERLC
AC_ERLANG_PATH_ERL
AC_ERLANG_SUBST_INSTALL_LIB_DIR

dnl AC_PATH_PROGS([ERLC],[erlc],,[$path])
dnl AC_PATH_PROGS([ERL],[erl],,[$path])
dnl AC_LANG_PUSH(C++)

AC_CONFIG_FILES([
		 Makefile
		 include/Makefile
		 src/Makefile
		 src/yate/Makefile
		 src/yate_demo/Makefile
		 src/esipua/Makefile
		 src/sipb2bua/Makefile
		 src/sipclient/Makefile
		])

AC_OUTPUT
