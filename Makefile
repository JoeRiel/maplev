# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

# {{{ Binaries

# emacs binary
EMACS = emacs-snapshot
CP = cp

# }}}
# {{{ Directories

# where local software is found
prefix = /usr/local

# where local lisp files go
lispdir = $(prefix)/share/emacs/site-lisp

# where info files go
infodir = $(prefix)/share/info

# where the maple archive goes
mapleinstalldir = $(HOME)/maple/lib

# }}}

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(progn \
                    (add-to-list (quote load-path) (expand-file-name \"./lisp\")) \
	            (add-to-list (quote load-path) \"$(lispdir)\"))"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = maplev

LISPFILES = $(ELS:%=%.el)
ELCFILES = $(LISPFILES:.el=.elc)

default: byte-compile
byte-compile: $(ELCFILES)
dist: maplev.zip
install: install-lisp

%.elc : %.el
	$(ELC) $<

install-lisp: $(LISPFILES) $(ELCFILES)
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) $+ $(lispdir)

maplev.zip: $(elfile) doc/maplev.texi
	zip $@ $?

.PHONY: p4put p4get
p4dir = /home/joe/work/MapleSoft/sandbox/groups/share/emacs/maplev
p4put: maplev.el 
	(cd $(p4dir); p4 edit $?)
	$(CP) $? $(p4dir)

p4get: 
	$(CP) $(pfdir)/. .

clean:
	$(RM) *.elc

.PHONY: byte-compile clean default dist install install-lisp

