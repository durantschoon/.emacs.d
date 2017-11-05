# Generated automatically from Makefile.in by configure.
# $Id$
#
# Makefile
#
# 'make all' makes texindex and makeinfo
# 'make doc' makes DVI files (User's Guide and Ref Card)
# 'make info' makes the on-line documentation
# 'make install' compiles the Lisp sources and puts
#	         everything in place
#
# Frank Wegmann, 1994
#

SHELL = /bin/sh

#### Start of system configuration section. ####

srcdir = .

CC = gcc
CFLAGS = -g

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

DEFS =  -DHAVE_STRING_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_UNISTD_H=1 -DSTDC_HEADERS=1 -DTIME_WITH_SYS_TIME=1 -DHAVE_ALLOCA_H=1 -DHAVE_ALLOCA=1

# The first Emacs which is found in the path will be used.
# If there are e.g. versions 18 and 19, you should specify
# the appropiate binary by hand.
EMACS = emacs

# Using Emacs in batch mode.
BATCH = $(EMACS) -batch -q lpath.el -f eval-current-buffer

# Specify the byte-compiler for compiling lisp files
ELC = $(BATCH) -f batch-byte-compile

TEX = nil
TEXI2DVI = $(srcdir)/texi2dvi

MAKE = make  # BSD doesn't have it as a default.

# The default location for installation.  Everything is placed in
# subdirectories of this directory.  The default values for many of
# the variables below are expressed in terms of this one, so you may
# not need to change them.  This defaults to /usr/local.
prefix=/usr/local

# Like `prefix', but used for architecture-specific files.
exec_prefix=${prefix}

# Where to install texindex and makeinfo binaries
bindir=${prefix}/bin

# Where to install architecture-independent data files.	 ${lispdir}
# and ${etcdir} are subdirectories of this.
datadir=${prefix}/lib

# Where to install and expect the info files describing tla-mode.
infodir=${prefix}/info

# Where to install Emacs lisp files
locallispdir=${datadir}/emacs/site-lisp

#### End of system configuration section. ####

TEXINDEX_OBJS = texindex.o getopt.o getopt1.o
MAKEINFO_OBJS = makeinfo.o getopt.o getopt1.o

LISPSRC= \
	easymenu.el reporter.el tla-mode.el

all:	texindex makeinfo

doc:	dvi info

dvi:	tla-mode.texi tla-ref.tex
	$(TEXI2DVI) tla-mode.texi
	$(TEX) tla-ref.tex

info:	makeinfo
	makeinfo tla-mode.texi

ps:	dvi
	dvips tla-mode.dvi
	dvips tla-ref.dvi	

install: info
	@echo "Byte compiling Lisp sources.
	@echo "Expect some harmless warnings about free variables and"
	@echo "undefined functions from the Emacs 19 byte-compiler."
	$(ELC) $(LISPSRC)
	$(INSTALL_DATA) *.elc ${locallispdir}
	$(INSTALL_DATA) tla-mode.info* ${infodir}
	@echo "Don't forget to include the info file entry in " ${infodir}"/dir"

makeinfo: ${MAKEINFO_OBJS}
	$(CC) -o makeinfo ${MAKEINFO_OBJS}

makeinfo.o: makeinfo.c
	$(CC) $(CFLAGS) $(DEFS) -DMAKEINFO_MAJOR=1 -DMAKEINFO_MINOR=0 -c makeinfo.c

texindex: ${TEXINDEX_OBJS}
	$(CC) -o texindex ${TEXINDEX_OBJS}

texindex.o: texindex.c
	$(CC) $(CFLAGS) $(DEFS) -c texindex.c
getopt.o: getopt.c getopt.h
	${CC} $(CFLAGS) $(DEFS) -c getopt.c
getopt1.o: getopt1.c getopt.h
	${CC} $(CFLAGS) $(DEFS) -c getopt1.c

Makefile: Makefile.in config.status
	$(SHELL) config.status

config.status: configure
	$(SHELL) config.status --recheck

configure: configure.in
	cd $(srcdir); autoconf

.PHONY: mostlyclean clean distclean realclean

mostlyclean:
	-rm -f core *.o *~

clean:	mostlyclean
	-rm -f texindex makeinfo

distclean: clean
	-rm -f config.status Makefile

realclean: distclean
	true
