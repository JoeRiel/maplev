# -*- mode: makefile-gmake; mode: folding -*-
# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL = /bin/sh

include help-system.mak

# {{{ Binaries

EMACS = emacs
MAPLE = maple
TEXI2HTML = makeinfo --html --number-sections
TEXI2PDF = texi2pdf

BROWSER = firefox
PDFVIEWER = evince
INFO = info

# }}}
# {{{ Directories

# where local software is found
prefix = /usr/local
exec_prefix = $(prefix)

# where local lisp files go
LISPDIR = $(DESTDIR)$(prefix)/share/emacs/site-lisp

# where info files go
INFODIR = $(DESTDIR)$(prefix)/share/info

# where the maple archive goes
MAPLEDIR = $(HOME)/maple/lib

# }}}
# {{{ Elisp

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(progn \
                    (add-to-list (quote load-path) (expand-file-name \"./lisp\")) \
	            (add-to-list (quote load-path) \"$(LISPDIR)\"))"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = maplev

LISPFILES = $(ELS:%=lisp/%.el)
ELCFILES = $(LISPFILES:.el=.elc)

%.elc : %.el
	$(ELC) $<

byte-compile: $(call print-help,byte-compile,Byte-compile the elisp)
byte-compile: $(ELCFILES)

clean-elisp: $(call print-help,clean-elisp,Remove byte-compiled files)
clean-elisp:
	rm -f $(ELCFILES)

.PHONY: byte-compile clean-elisp

# }}}
# {{{ Documentation

INFOFILES = doc/maplev
PDFFILES  = doc/maplev.pdf
TEXIFILES = doc/maplev.texi doc/version.texi
HTMLFILES = doc/maplev.html

DOCFILES = $(TEXIFILES) $(INFOFILES) $(PDFFILES) $(HTMLFILES)

doc: $(call print-help,doc,Create the info and pdf documentation)
doc:  info pdf
info: $(call print-help,info,Create info file)
info: doc/maplev
pdf:  $(call print-help,pdf,Create pdf documentation)
pdf:  doc/maplev.pdf
html:  $(call print-help,html,Create html documentation)
html: doc/maplev.html

doc/maplev.pdf: doc/maplev.texi doc/version.texi
	(cd doc; $(TEXI2PDF) maplev.texi)

doc/maplev: doc/maplev.texi doc/version.texi
	(cd doc; $(MAKEINFO) --no-split maplev.texi --output=maplev)

doc/maplev.html: doc/maplev.texi doc/version.texi
	(cd doc; $(TEXI2HTML) --no-split -o maplev.html maplev.texi)

clean-doc: $(call print-help,clean-doc,Remove the auxiliary files in doc)
clean-doc:
	rm -f $(filter-out $(TEXIFILES) $(DOCFILES) $(INFOFILES), $(wildcard doc/*))

clean-doc-all: $(call print-help,clean-doc-all,Remove all generated documentation)
clean-doc-all: clean-doc
	rm -f $(INFOFILES) $(PDFFILES) $(HTMLFILES)

.PHONY: doc html info pdf clean-doc clean-doc-all p i h

# preview pdf
p: $(call print-help,p,Preview the pdf)
p: doc/maplev.pdf
	$(PDFVIEWER) $<

# preview info
i: $(call print-help,i,Preview the info)
i: doc/maplev
	$(INFOVIEWER) $<

h: $(call print-help,h,Preview the html)
h: doc/maplev.html
	$(BROWSER) $<

# }}}
# {{{ Maple

MAPLEFILES = maple/maplev.mpl

mla = $(MAPLEFILES:.mpl=.mla)

maple: $(call print-help,maple,Create the Maple archive)
maple: $(mla)

%.mla : %.mpl
	rm -f $@
	echo "LibraryTools:-Save('maplev',\"$@\");" | cat $^ - | ${MAPLE} -q

clean-maple:
	rm -f $(mla)

.PHONY: maple clean-maple

# }}}
# {{{ Installation

MKDIR = if test ! -d $(1); then mkdir --parents $(1); fi

install: $(call print-help,install,Install lisp and doc)
install: install-lisp install-info

install-lisp: $(call print-help,install-lisp,Install lisp in $(subst $(DESTDIR)$(prefix),$$DESTDIR/$$prefix,$(LISPDIR)))
install-lisp: $(LISPFILES) $(ELCFILES)
	$(call MKDIR,$(LISPDIR))
	cp $+ $(LISPDIR)

install-info: $(call print-help,install-info,Install info files in $(subst $(DESTDIR)$(prefix),$$DESTDIR/$$prefix,$(INFODIR)) and update dir)
install-info: $(INFOFILES)
	$(POST_INSTALL)
	$(call MKDIR,$(INFODIR))
	cp $(INFOFILES) $(INFODIR)
	for file in $(INFOFILES); do install-info --info-dir=$(INFODIR) $${file}; done

install-maple: $(call print-help,install-maple,Install mla in $$MAPLEDIR)
install-maple: $(mla)
	$(call MKDIR,$(MAPLEDIR))
	cp --archive $+ $(MAPLEDIR)

clean-install: $(call print-help,clean-install,Remove installed files)
clean-install:
	rm -f $(addprefix $(LISPDIR),$(lispfiles) $(ELCFILES))
	rm -f $(addprefix $(INFODIR),$(INFOFILES))
	rm -f $(addprefix $(MAPLEDIR),$(mla))


.PHONY: install install-lisp install-info install-maple clean-install

# }}}
# {{{ Distribution


DISTFILES_extra = ChangeLog Copyright Makefile help-system.mak README

src = lisp/maplev.el doc/maplev.texi doc/version.texi maple/maplev.mpl

zip: maplev.zip

maplev.zip: $(elfile) doc/maplev.texi
	zip $@ $?

dist: $(call print-help,dist,Create maplev-$$TAG.tar.gz file)
dist: $(LISPFILES) $(TEXIFILES)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	rm -rf maplev-$(TAG)
	$(call MKDIR,maplev-$(TAG))
	$(call MKDIR,maplev-$(TAG)/doc)
	$(call MKDIR,maplev-$(TAG)/lisp)
	$(call MKDIR,maplev-$(TAG)/maplev)
	cp $(LISPFILES) maplev-$(TAG)/lisp
	cp $(TEXIFILES) maplev-$(TAG)/doc
	cp $(MAPLEFILES) maplev-$(TAG)/maple
	cp $(DISTFILES_extra) maplev-$(TAG)/
	zip -r maplev-$(TAG).zip maplev-$(TAG)
	tar zcvf maplev-$(TAG).tar.gz maplev-$(TAG)

.PHONY:  dist zip

# }}}
# {{{ P4

p4dir = /home/joe/work/MapleSoft/sandbox/groups/share/emacs/maplev
p4put: maplev.el 
	(cd $(p4dir); p4 edit $?)
	cp $? $(p4dir)

p4get: 
	cp $(pfdir)/. .

.PHONY: p4put p4get

# }}}
# {{{ Clean

clean: $(call print-help,clean,Remove created and aux files)
clean: clean-elisp clean-maple clean-doc

.PHONY: clean

# }}}