# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL = /bin/sh

PKG := maplev
VERSION := 2.19

include help-system.mak

# {{{ Executables

EMACS := emacs
MAPLE := maple
TEXI2HTML := makeinfo --html --number-sections
TEXI2PDF := texi2pdf

BROWSER := firefox
CP := cp --archive
PDFVIEWER := evince
INFO := info

# }}}
# {{{ Directories

# where local lisp files go.  
LISP-DIR  := $(HOME)/.emacs.d/maple

# where info files go
INFO-DIR = $(HOME)/share/info

# where the Maple archive goes
MAPLE_LIB_DIR := $(HOME)/maple/toolbox/emacs/lib

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
	$(RM) $(ELC-FILES)

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
	$(RM) $(filter-out $(TEXI-FILES) $(DOC-FILES) $(INFO-FILES) doc/fdl.texi, $(wildcard doc/*))

clean-doc-all: $(call print-help,clean-doc-all,Remove all generated documentation)
clean-doc-all: clean-doc
	$(RM) $(INFO-FILES) $(PDF-FILES) $(HTML-FILES)

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
# {{{ Maple Archive (mla)

.PHONY: mla 
mla := maplev.mla
mla: $(call print-help,mla,Create Maple archive: $(mla))
mla: $(mla)

txtbold   := $(shell tput bold)
txtred    := $(shell tput setaf 1)
txtnormal := $(shell tput sgr0)
warn = "$(txtred)$(textbold)$1$(txtnormal)"

%.mla: maple/%.mpl
	@$(RM) $@
	@echo "Building Maple archive $@"
	@err=$$($(MAPLE) -q -I maple -D BUILD_MLA $< ) ; \
		if [ ! -z "$$err" ]; then \
			echo $(call warn,$$err); \
		fi

# }}}
# {{{ Installation

MKDIR = if test ! -d $(1); then mkdir --parents $(1); fi

install: $(call print-help,install,Install everything)
install: $(addprefix install-,info lisp mla)

install-lisp: $(call print-help,install-lisp,Install lisp in $(LISP-DIR))
install-lisp: $(LISP-FILES) $(ELC-FILES)
	@$(call MKDIR,$(LISP-DIR))
	@$(CP) $+ $(LISP-DIR)

install-info: $(call print-help,install-info,Install info files in $(INFO-DIR))
install-info: $(INFO-FILES)
	@$(call MKDIR,$(INFO-DIR))
	$(CP) $(INFO-FILES) $(INFO-DIR)
	@echo Update 'dir' node:
	@for file in $(INFO-FILES); do ginstall-info --info-dir=$(INFO-DIR) $${file}; done

install-mla: $(call print-help,install-mla,Install mla in $(MAPLE_LIB_DIR))
install-mla: $(mla)
	@$(call MKDIR,$(MAPLE_LIB_DIR))
	@echo "Installing Maple archive $(mla) into $(MAPLE_LIB_DIR)/"
	@$(CP) $+ $(MAPLE_LIB_DIR)

clean-install: $(call print-help,clean-install,Remove installed files)
clean-install:
	$(RM) $(addprefix $(LISP-DIR)/,$(PKG).*)
	$(RM) $(addprefix $(INFO-DIR)/,$(PKG))
	$(RM) -r $(MAPLE_LIB_DIR)/$(mla)


.PHONY: install install-lisp install-mla install-info clean-install

# }}}
# {{{ Distribution

DIST_extra = Copyright README RELEASE-NOTES install

DIST-FILES_extra = ChangeLog Makefile help-system.mak

src = lisp/$(PKG).el doc/$(PKG).texi doc/version.texi

dist: $(call print-help,dist,Create $(PKG)-$$TAG.tar.gz file)
dist: $(LISP-FILES) $(TEXI-FILES)
	$(RM) -r $(PKG)-$(VERSION)
	$(call MKDIR,$(PKG)-$(VERSION))
	$(call MKDIR,$(PKG)-$(VERSION)/doc)
	$(call MKDIR,$(PKG)-$(VERSION)/lisp)
	$(call MKDIR,$(PKG)-$(VERSION)/$(PKG))
	$(CP) $(LISP-FILES) $(PKG)-$(VERSION)/lisp
	$(CP) $(TEXI-FILES) $(PKG)-$(VERSION)/doc
	$(CP) $(DIST-FILES_extra) $(DIST_extra) $(PKG)-$(VERSION)/
	zip -r $(PKG)-$(VERSION).zip $(PKG)-$(VERSION)
	tar zcvf $(PKG)-$(VERSION).tar.gz $(PKG)-$(VERSION)

.PHONY:  dist

# }}}
# {{{ P4

p4dir = /home/joe/work/MapleSoft/sandbox/groups/share/emacs/$(PKG)
p4put: $(PKG).el 
	(cd $(p4dir); p4 edit $?)
	$(CP) $? $(p4dir)

p4get: 
	$(CP) $(pfdir)/. .

.PHONY: p4put p4get

# }}}
# {{{ Clean

clean: $(call print-help,clean,Remove created and aux files)
clean: clean-elisp clean-doc

.PHONY: clean

# }}}o