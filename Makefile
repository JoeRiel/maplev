# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL = /bin/sh

PKG := maplev
VERSION := 2.29

include help-system.mak

# {{{ Executables

EMACS := emacs
MAPLE := cmaple
TEXI2HTML := makeinfo --html --number-sections
TEXI2PDF := texi2pdf

BROWSER := x-www-browser
CP := cp --archive
PDFVIEWER := xpdf
INFO := info

# }}}
# {{{ Directories

# where local lisp files go.  
LISP-DIR  := $(HOME)/.emacs.d/maple

# where info files go
INFO-DIR = $(HOME)/share/info

# where the Maple archive goes
MAPLE-LIB-DIR := $(HOME)/maple/toolbox/emacs/lib

# }}}
# {{{ Auxiliary functions (warn, shellerr)

txtbold   := $(shell tput bold)
# 0=black 1=red 2=green 3=yellow 4=blue 5=magenta 6=cyan 7=white
txthilite := $(shell tput setaf 3)
txtnormal := $(shell tput sgr0)
warn = "$(txthilite)$1$(txtnormal)"
shellerr = $(call showerr,$1 2>&1 > /dev/null)
showerr = err="$$($1)" ; if [ "$$err" ]; then echo $(call warn,$$err); fi

# }}}

.PHONY: all 

COMMA := ,

all: $(call print-help,all,	Create mla$(COMMA) elcs$(COMMA) and info)
all: byte-compile mla info


# {{{ Elisp

help: $(call print-separator)

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(progn \
			(add-to-list (quote load-path) (expand-file-name \"./lisp\")) \
			(add-to-list (quote load-path) \"$(LISP-DIR)\") \
			(delete \"/usr/share/emacs/23.3/site-lisp/emacs-goodies-el\" load-path))"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = $(PKG) $(addprefix $(PKG),-cmaple -common -custom -help -history -indent -mint -proc -re -utils) button-lock

LISP-FILES = $(ELS:%=lisp/%.el)
ELC-FILES = $(LISP-FILES:.el=.elc)

%.elc : %.el
	@$(RM) $@
	@echo Byte-compiling $+
	@$(call showerr,$(ELC) $< 2>&1 > /dev/null | sed '/^Wrote/d')

byte-compile: $(call print-help,byte-compile,Byte-compile $$(LISP-FILES))
byte-compile: $(ELC-FILES)

clean-elisp: $(call print-help,clean-elisp,Remove byte-compiled files)
clean-elisp:
	$(RM) $(ELC-FILES)

lisp-install: $(call print-help,lisp-install,Install lisp in $(LISP-DIR))
lisp-install: $(LISP-FILES) $(ELC-FILES)
	@$(call MKDIR,$(LISP-DIR))
	$(CP) $+ $(LISP-DIR)

links-install: $(call print-help,links-install,Install links to the lisp files)
links-install: $(LISP-FILES) $(ELC-FILES)
	@$(call MKDIR,$(LISP-DIR))
	@ln -nfst $(LISP-DIR) $(realpath $^)

links-uninstall: $(call print-help,links-uninstall,Remove links to the lisp files)
links-uninstall:
	$(RM) $(addprefix $(LISP-DIR)/,$(notdir $(LISP-FILES) $(ELC-FILES)))

.PHONY: byte-compile clean-elisp links-install links-uninstall

# }}}
# {{{ Documentation

help: $(call print-separator)

INFO-FILES = doc/$(PKG).info
PDF-FILES  = doc/$(PKG).pdf
TEXI-FILES = doc/$(PKG).texi doc/version.texi
HTML-FILES = doc/$(PKG).html

DOC-FILES = $(TEXI-FILES) $(INFO-FILES) $(PDF-FILES) $(HTML-FILES)

doc: $(call print-help,doc,	Create the info and html documentation)
doc:  info html
info: $(call print-help,info,	Create info file)
info: doc/$(PKG).info
pdf:  $(call print-help,pdf,	Create pdf documentation)
pdf:  doc/$(PKG).pdf
html:  $(call print-help,html,	Create html documentation)
html: doc/$(PKG).html

doc/$(PKG).pdf: doc/$(PKG).texi doc/version.texi
	(cd doc; $(TEXI2PDF) $(PKG).texi)

doc/$(PKG).info: doc/$(PKG).texi doc/version.texi
	(cd doc; $(MAKEINFO) --no-split $(PKG).texi --output=$(PKG).info)

doc/$(PKG).html: doc/$(PKG).texi doc/version.texi
	(cd doc; $(TEXI2HTML) --no-split -o $(PKG).html $(PKG).texi)

clean-doc: $(call print-help,clean-doc,Remove the auxiliary files in doc)
clean-doc:
	$(RM) $(filter-out $(TEXI-FILES) $(DOC-FILES) $(INFO-FILES) doc/fdl.texi, $(wildcard doc/*))

clean-doc-all: $(call print-help,clean-doc-all,Remove all generated documentation)
clean-doc-all: clean-doc
	$(RM) $(INFO-FILES) $(PDF-FILES) $(HTML-FILES)

info-install: $(call print-help,info-install,Install info files in $(INFO-DIR))
info-install: $(INFO-FILES)
	@$(call MKDIR,$(INFO-DIR))
	$(CP) $(INFO-FILES) $(INFO-DIR)
	@echo Update 'dir' node
	@for file in $(INFO-FILES); do ginstall-info --info-dir=$(INFO-DIR) $${file}; done

.PHONY: doc html info pdf clean-doc clean-doc-all p i h info-install

# preview pdf
p: $(call print-help,p,	Preview the pdf)
p: doc/$(PKG).pdf
	$(PDFVIEWER) $<

# preview info
i: $(call print-help,i,	Preview the info)
i: doc/$(PKG)
	$(INFOVIEWER) $<

# preview html
h: $(call print-help,h,	Preview the html)
h: doc/$(PKG).html
	$(BROWSER) $<

# }}}
# {{{ Maple Archive (mla)

help: $(call print-separator)

.PHONY: mla mla-install mla-clean
mla := maplev.mla
mla: $(call print-help,mla,	Create Maple archive: $(mla))
mla: $(mla)

%.mla: maple/%.mpl
	@$(RM) $@
	@echo "Building Maple archive $@"
	@err=$$($(MAPLE) -q -I maple -D BUILD-MLA $< ) ; \
		if [ ! -z "$$err" ]; then \
			echo $(call warn,$$err); \
		fi

mla-install: $(call print-help,mla-install,Install mla in $(MAPLE-LIB-DIR))
mla-install: $(mla)
	@$(call MKDIR,$(MAPLE-LIB-DIR))
	@echo "Installing Maple archive $(mla) into $(MAPLE-LIB-DIR)/"
	@$(CP) $+ $(MAPLE-LIB-DIR)

mla-clean: $(call print-help,mla-clean,Remove $(mla))
mla-clean:
	$(RM) $(mla)


# }}}
# {{{ Installation

help: $(call print-separator)

MKDIR = if test ! -d $(1); then mkdir --parents $(1); fi

install: $(call print-help,install,	Install everything)
install: $(addsuffix -install,info lisp mla)

clean-install: $(call print-help,clean-install,Remove installed files)
clean-install: links-uninstall
	$(RM) $(addprefix $(LISP-DIR)/,$(PKG).*)
	$(RM) $(addprefix $(INFO-DIR)/,$(PKG))
	$(RM) -r $(MAPLE-LIB-DIR)/$(mla)

.PHONY: install clean-install

# }}}
# {{{ Distribution

help: $(call print-separator)

DIST-extra = Copyright README RELEASE-NOTES install

DIST-FILES-extra = ChangeLog Makefile /usr/local/include/help-system.mak

src = lisp/$(PKG).el doc/$(PKG).texi doc/version.texi

dist: $(call print-help,dist,	Create $(PKG)-$$TAG.tar.gz file)
dist: $(LISP-FILES) $(TEXI-FILES)
	$(RM) -r $(PKG)-$(VERSION)
	$(call MKDIR,$(PKG)-$(VERSION))
	$(call MKDIR,$(PKG)-$(VERSION)/doc)
	$(call MKDIR,$(PKG)-$(VERSION)/lisp)
	$(call MKDIR,$(PKG)-$(VERSION)/$(PKG))
	$(CP) $(LISP-FILES) $(PKG)-$(VERSION)/lisp
	$(CP) $(TEXI-FILES) $(PKG)-$(VERSION)/doc
	$(CP) $(DIST-FILES-extra) $(DIST-extra) $(PKG)-$(VERSION)/
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

help: $(call print-separator)

clean: $(call print-help,clean,	Remove created and aux files)
clean: clean-elisp clean-doc mla-clean

.PHONY: clean

# }}}
