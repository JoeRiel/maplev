# -*- mode: makefile-gmake; mode: folding -*-
# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL = /bin/sh

VERSION := 2.17

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
LISP-DIR = $(DEST-DIR)$(prefix)/share/emacs/site-lisp

# where info files go
INFO-DIR = $(DEST-DIR)$(prefix)/share/info

# where the maple archive goes
MAPLE-DIR = $(HOME)/maple/lib

# }}}
# {{{ Elisp

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(progn \
                    (add-to-list (quote load-path) (expand-file-name \"./lisp\")) \
	            (add-to-list (quote load-path) \"$(LISP-DIR)\"))"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = maplev

LISP-FILES = $(ELS:%=lisp/%.el)
ELC-FILES = $(LISP-FILES:.el=.elc)

%.elc : %.el
	$(ELC) $<

byte-compile: $(call print-help,byte-compile,Byte-compile the elisp)
byte-compile: $(ELC-FILES)

clean-elisp: $(call print-help,clean-elisp,Remove byte-compiled files)
clean-elisp:
	rm -f $(ELC-FILES)

.PHONY: byte-compile clean-elisp

# }}}
# {{{ Documentation

INFO-FILES = doc/maplev
PDF-FILES  = doc/maplev.pdf
TEXI-FILES = doc/maplev.texi doc/version.texi
HTML-FILES = doc/maplev.html

DOC-FILES = $(TEXI-FILES) $(INFO-FILES) $(PDF-FILES) $(HTML-FILES)

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
	rm -f $(filter-out $(TEXI-FILES) $(DOC-FILES) $(INFO-FILES), $(wildcard doc/*))

clean-doc-all: $(call print-help,clean-doc-all,Remove all generated documentation)
clean-doc-all: clean-doc
	rm -f $(INFO-FILES) $(PDF-FILES) $(HTML-FILES)

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

MAPLE-FILES = maple/maplev.mpl

mla = $(MAPLE-FILES:.mpl=.mla)

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

install-all: $(call print-help,install-all,Install lisp and doc)
install-all: install-lisp install-info

install-lisp: $(call print-help,install-lisp,Install lisp in $(subst $(DEST-DIR)$(prefix),$$DEST-DIR/$$prefix,$(LISP-DIR)))
install-lisp: $(LISP-FILES) $(ELC-FILES)
	$(call MKDIR,$(LISP-DIR))
	cp $+ $(LISP-DIR)

install-info: $(call print-help,install-info,Install info files in $(subst $(DEST-DIR)$(prefix),$$DEST-DIR/$$prefix,$(INFO-DIR)) and update dir)
install-info: $(INFO-FILES)
	$(POST_INSTALL)
	$(call MKDIR,$(INFO-DIR))
	cp $(INFO-FILES) $(INFO-DIR)
	for file in $(INFO-FILES); do install-info --info-dir=$(INFO-DIR) $${file}; done

install-maple: $(call print-help,install-maple,Install mla in $$MAPLE-DIR)
install-maple: $(mla)
	$(call MKDIR,$(MAPLE-DIR))
	cp --archive $+ $(MAPLE-DIR)

clean-install: $(call print-help,clean-install,Remove installed files)
clean-install:
	rm -f $(addprefix $(LISP-DIR),$(lispfiles) $(ELC-FILES))
	rm -f $(addprefix $(INFO-DIR),$(INFO-FILES))
	rm -f $(addprefix $(MAPLE-DIR),$(mla))


.PHONY: install-all install-lisp install-info install-maple clean-install

# }}}
# {{{ Distribution


DIST-FILES_extra = ChangeLog Copyright Makefile help-system.mak README

src = lisp/maplev.el doc/maplev.texi doc/version.texi maple/maplev.mpl

dist: $(call print-help,dist,Create maplev-$$TAG.tar.gz file)
dist: $(LISP-FILES) $(TEXI-FILES)
	rm -rf maplev-$(VERSION)
	$(call MKDIR,maplev-$(VERSION))
	$(call MKDIR,maplev-$(VERSION)/doc)
	$(call MKDIR,maplev-$(VERSION)/lisp)
	$(call MKDIR,maplev-$(VERSION)/maplev)
	cp $(LISP-FILES) maplev-$(VERSION)/lisp
	cp $(TEXI-FILES) maplev-$(VERSION)/doc
	cp $(MAPLE-FILES) maplev-$(VERSION)/maple
	cp $(DIST-FILES_extra) maplev-$(VERSION)/
	zip -r maplev-$(VERSION).zip maplev-$(VERSION)
	tar zcvf maplev-$(VERSION).tar.gz maplev-$(VERSION)

dist := $(LISP-FILES) $(INFO-FILES) $(HTML-FILES) README install
zip: $(dist)
	rm -f maplev-$(VERSION).zip
	zip maplev-$(VERSION).zip $+

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