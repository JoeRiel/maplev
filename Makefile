# -*- mode: makefile-gmake; mode: folding -*-
# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL = /bin/sh

PKG := maplev
VERSION := 2.19

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

# where local lisp files go.  
LISP-DIR  := $(HOME)/.emacs.d/$(PKG)

# where info files go
INFO-DIR = $(HOME)/share/info

# }}}
# {{{ Elisp

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(progn \
                    (add-to-list (quote load-path) (expand-file-name \"./lisp\")) \
	            (add-to-list (quote load-path) \"$(LISP-DIR)\"))"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = $(PKG)

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

INFO-FILES = doc/$(PKG)
PDF-FILES  = doc/$(PKG).pdf
TEXI-FILES = doc/$(PKG).texi doc/version.texi
HTML-FILES = doc/$(PKG).html

DOC-FILES = $(TEXI-FILES) $(INFO-FILES) $(PDF-FILES) $(HTML-FILES)

doc: $(call print-help,doc,Create the info and pdf documentation)
doc:  info pdf
info: $(call print-help,info,Create info file)
info: doc/$(PKG)
pdf:  $(call print-help,pdf,Create pdf documentation)
pdf:  doc/$(PKG).pdf
html:  $(call print-help,html,Create html documentation)
html: doc/$(PKG).html

doc/$(PKG).pdf: doc/$(PKG).texi doc/version.texi
	(cd doc; $(TEXI2PDF) $(PKG).texi)

doc/$(PKG): doc/$(PKG).texi doc/version.texi
	(cd doc; $(MAKEINFO) --no-split $(PKG).texi --output=$(PKG))

doc/$(PKG).html: doc/$(PKG).texi doc/version.texi
	(cd doc; $(TEXI2HTML) --no-split -o $(PKG).html $(PKG).texi)

clean-doc: $(call print-help,clean-doc,Remove the auxiliary files in doc)
clean-doc:
	rm -f $(filter-out $(TEXI-FILES) $(DOC-FILES) $(INFO-FILES) fdl.texi, $(wildcard doc/*))

clean-doc-all: $(call print-help,clean-doc-all,Remove all generated documentation)
clean-doc-all: clean-doc
	rm -f $(INFO-FILES) $(PDF-FILES) $(HTML-FILES)

.PHONY: doc html info pdf clean-doc clean-doc-all p i h

# preview pdf
p: $(call print-help,p,Preview the pdf)
p: doc/$(PKG).pdf
	$(PDFVIEWER) $<

# preview info
i: $(call print-help,i,Preview the info)
i: doc/$(PKG)
	$(INFOVIEWER) $<

h: $(call print-help,h,Preview the html)
h: doc/$(PKG).html
	$(BROWSER) $<

# }}}
# {{{ Installation

MKDIR = if test ! -d $(1); then mkdir --parents $(1); fi

install-all: $(call print-help,install-all,Install lisp and doc)
install-all: install-lisp install-info

install-lisp: $(call print-help,install-lisp,Install lisp in $(subst $(DEST-DIR)$(prefix),$$DEST-DIR/$$prefix,$(LISP-DIR)))
install-lisp: $(LISP-FILES) $(ELC-FILES)
	@$(call MKDIR,$(LISP-DIR))
	cp $+ $(LISP-DIR)

install-info: $(call print-help,install-info,Install info files in $(subst $(DEST-DIR)$(prefix),$$DEST-DIR/$$prefix,$(INFO-DIR)) and update dir)
install-info: $(INFO-FILES)
	@$(call MKDIR,$(INFO-DIR))
	cp $(INFO-FILES) $(INFO-DIR)
	@echo Update 'dir' node:
	@for file in $(INFO-FILES); do ginstall-info --info-dir=$(INFO-DIR) $${file}; done

clean-install: $(call print-help,clean-install,Remove installed files)
clean-install:
	rm -f $(addprefix $(LISP-DIR)/,$(PKG).*)
	rm -f $(addprefix $(INFO-DIR)/,$(PKG))


.PHONY: install-all install-lisp install-info clean-install

# }}}
# {{{ Distribution

DIST_extra = Copyright README RELEASE-NOTES install

DIST-FILES_extra = ChangeLog Makefile help-system.mak

src = lisp/$(PKG).el doc/$(PKG).texi doc/version.texi

dist: $(call print-help,dist,Create $(PKG)-$$TAG.tar.gz file)
dist: $(LISP-FILES) $(TEXI-FILES)
	rm -rf $(PKG)-$(VERSION)
	$(call MKDIR,$(PKG)-$(VERSION))
	$(call MKDIR,$(PKG)-$(VERSION)/doc)
	$(call MKDIR,$(PKG)-$(VERSION)/lisp)
	$(call MKDIR,$(PKG)-$(VERSION)/$(PKG))
	cp $(LISP-FILES) $(PKG)-$(VERSION)/lisp
	cp $(TEXI-FILES) $(PKG)-$(VERSION)/doc
	cp $(DIST-FILES_extra) $(DIST_extra) $(PKG)-$(VERSION)/
	zip -r $(PKG)-$(VERSION).zip $(PKG)-$(VERSION)
	tar zcvf $(PKG)-$(VERSION).tar.gz $(PKG)-$(VERSION)

.PHONY:  dist

# }}}
# {{{ P4

p4dir = /home/joe/work/MapleSoft/sandbox/groups/share/emacs/$(PKG)
p4put: $(PKG).el 
	(cd $(p4dir); p4 edit $?)
	cp $? $(p4dir)

p4get: 
	cp $(pfdir)/. .

.PHONY: p4put p4get

# }}}
# {{{ Clean

clean: $(call print-help,clean,Remove created and aux files)
clean: clean-elisp clean-doc

.PHONY: clean

# }}}