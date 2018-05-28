# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL = /bin/bash

PKG := maplev
pkg := $(PKG)

# {{{ Version

ifneq ($(wildcard .git),)
  GIT-BRANCH = $(shell git rev-parse --abbrev-ref HEAD)
  ifdef RELEASE
    VERSION = $(RELEASE)
  else
    VERSION = $(subst release-,,$(shell git describe --match release\* --abbrev=0 HEAD))
  endif
else
  GIT-BRANCH  = N/A
  VERSION     = N/A
endif
DATE = $(shell date "+%d %B %Y")

# }}}

include help-system.mak

# {{{ Executables

EMACS := emacs
# cmaple is better for production release
MAPLE := maple
TEXI2HTML := $(MAKEINFO) --html --number-sections
# TEXI2PDF := texi2pdf
TEXI2PDF := $(MAKEINFO) --pdf

BROWSER := x-www-browser
CP := cp --archive --verbose
PDFVIEWER := xpdf
INFO := info
INFOVIEWER := info

# }}}
# {{{ Directories

# where local lisp files go.  
LISP-DIR  := $(HOME)/.emacs.d/maple

# where info files go
INFO-DIR = $(HOME)/share/info

TOOLBOX-DIR := $(HOME)/maple/toolbox/maplev

# where the Maple archive goes
MAPLE-LIB-DIR := $(TOOLBOX-DIR)/lib

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
			(add-to-list (quote load-path) (expand-file-name \".emacs.d/el-get/find-file-in-project\" \"$(HOME)\")) \
			(delete \"/usr/share/emacs/23.3/site-lisp/emacs-goodies-el\" load-path))"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

LISP-VERSION := lisp/maplev-version.el
LISP-RELEASE := lisp/maplev-release.el
EL-FILES = $(wildcard lisp/*.el) $(LISP-VERSION) $(LISP-RELEASE)
# EL-FILES-NO-VERSION = $(filter-out $(LISP-VERSION),$(EL-FILES))
# LISP-FILES = $(ELS:%=lisp/%.el)
ELC-FILES = $(EL-FILES:.el=.elc)

$(LISP-RELEASE): $(filter-out $(LISP-RELEASE) $(LISP-VERSION),$(EL-FILES))
	@lisp/MakeVersion $@ $(VERSION)

%.elc : %.el
	@$(RM) $@
	@echo Byte-compiling $^
	@$(call showerr,$(ELC) $< 2>&1 > /dev/null | sed '/^Wrote/d')

byte-compile: $(call print-help,byte-compile,Byte-compile $$(EL-FILES))
byte-compile: $(EL-FILES) $(ELC-FILES)

lisp-clean: $(call print-help,lisp-clean,Remove byte-compiled files)
lisp-clean:
	$(RM) $(ELC-FILES)

lisp-install: $(call print-help,lisp-install,Install lisp in $(LISP-DIR))
lisp-install: $(EL-FILES) $(ELC-FILES)
	@$(call MKDIR,$(LISP-DIR))
	$(CP) $^ $(LISP-DIR)

lisp-uninstall: $(call print-help,lisp-uninstall,Remove installed lisp files)
lisp-uninstall:
	@echo "removing installed lisp files"
	@$(RM) $(addprefix $(LISP-DIR)/,$(notdir $(EL-FILES) $(ELC-FILES)))

links-install: $(call print-help,links-install,Install links to the lisp files)
links-install: $(EL-FILES) $(ELC-FILES)
	@$(call MKDIR,$(LISP-DIR))
	@ln -nfst $(LISP-DIR) $(realpath $^)

.PHONY: byte-compile lisp-clean links-install lisp-install lisp-uninstall

# }}}
# {{{ Documentation

help: $(call print-separator)

TEXI-VERSION = doc/version.texi

INFO-FILE = doc/$(PKG).info
PDF-FILE  = doc/$(PKG).pdf
TEXI-FILES = doc/$(PKG).texi $(TEXI-VERSION)
HTML-FILE = doc/$(PKG).html

DOC-FILES = $(TEXI-FILES) $(INFO-FILE) $(PDF-FILE) $(HTML-FILE)

doc: $(call print-help,doc,	Create the info and html documentation)
doc:  info html
info: $(call print-help,info,	Create info file)
info: doc/$(PKG).info
pdf:  $(call print-help,pdf,	Create pdf documentation)
pdf:  doc/$(PKG).pdf
html:  $(call print-help,html,	Create html documentation)
html: doc/$(PKG).html


doc/$(PKG).pdf: doc/$(PKG).texi $(TEXI-VERSION)
	(cd doc; $(TEXI2PDF) $(PKG).texi)

doc/$(PKG).info: doc/$(PKG).texi $(TEXI-VERSION)
	(cd doc; $(MAKEINFO) \
	  --no-split $(PKG).texi \
	  --output=$(PKG).info)

doc/$(PKG).html: doc/$(PKG).texi $(TEXI-VERSION)
	(cd doc; $(TEXI2HTML) --no-split -o $(PKG).html $(PKG).texi)

doc-clean: $(call print-help,doc-clean,Remove the auxiliary files in doc)
doc-clean:
	$(RM) $(filter-out $(TEXI-FILES) $(DOC-FILES) $(INFO-FILE) doc/MakeVersion doc/fdl.texi, $(wildcard doc/*))

doc-clean-all: $(call print-help,doc-clean-all,Remove all generated documentation)
doc-clean-all: doc-clean
	$(RM) $(INFO-FILE) $(PDF-FILE) $(HTML-FILE)

info-install: $(call print-help,info-install,Install info files in $(INFO-DIR))
info-install: $(INFO-FILE)
	@$(call MKDIR,$(INFO-DIR))
	$(CP) $(INFO-FILE) $(INFO-DIR)
	@echo Update 'dir' node
	@for file in $(INFO-FILE); do ginstall-info --info-dir=$(INFO-DIR) $${file}; done

.PHONY: doc html info pdf doc-clean doc-clean-all p i h info-install

# preview pdf
p: $(call print-help,p,	Preview the pdf)
p: doc/$(PKG).pdf
	$(PDFVIEWER) $<

# preview info
i: $(call print-help,i,	Preview the info)
i: doc/$(PKG).info
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

MAPLE-FILES = maple/src/maplev.mpl $(wildcard maple/src/*.mm)

%.mla: $(MAPLE-FILES)
	@$(RM) $@
	@echo "Building Maple archive $@"
	mload --quiet --lineinfo --reindex --readonly \
	  --include=$(CURDIR)/maple/src \
	  --mla=$@ $<

mla-install: $(call print-help,mla-install,Install mla in $(MAPLE-LIB-DIR))
mla-install: $(mla)
	@$(call MKDIR,$(MAPLE-LIB-DIR))
	@echo "Installing Maple archive $(mla) into $(MAPLE-LIB-DIR)/"
	@$(CP) $^ $(MAPLE-LIB-DIR)

mla-clean: $(call print-help,mla-clean,Remove $(mla))
mla-clean:
	$(RM) $(mla)

# }}}
# {{{ hlp

help: $(call print-separator)

.PHONY: hlp hlp-install remove-preview

remove-preview :
	@$(RM) maple/src/_preview_.mm

hlp := $(pkg).help
hlp: $(call print-help,hlp,	Create Maple help database: $(hlp))
hlp: $(mla-installed) remove-preview $(pkg).help

hlp-installed := $(MAPLE-LIB-DIR)/$(hlp)

$(pkg).help : maple/src/$(pkg).mpl $(mms) $(mds) maple/include/mpldoc_macros.mpi
	@echo "Creating Maple help database"
	@$(RM) maple/src/_preview_.mm
	@$(call showerr,mpldoc --config nightly $^ 2>&1 | sed -n '/Warning:/{p;n};/Error:/p')
	@mhelp --replace $(pkg)

hlp-install: $(call print-help,hlp-install,Install $(hlp) in $(MAPLE-INSTALL-DIR))
hlp-install: $(hlp-installed)

$(hlp-installed): $(hlp)
	@$(MKDIR) $(MAPLE-INSTALL-DIR)
	@$(CP) --verbose $+ $@

# }}}
# {{{ book

help: $(call print-separator)

.PHONY: book book-install book-uninstall intro

book: $(call print-help,book,	Create ${pkg}.maple)

intro: $(INTRO)
INTRO := maple/mhelp/Intro.mw

$(INTRO): maple/src/Intro.md
	mpldoc --config nightly $<

book := $(PKG).maple
book: $(book)
$(book): $(mla) $(hlp) $(INFO-FILE) $(INTRO) Makefile
	$(RM) $@
	echo '(MakeBook)("$@" \
	                 , "$(mla)" \
	                 , "$(hlp)" \
	                 , "$(INFO-FILE)" \
	                 , "$(INTRO)" \
	                ):' \
	     | cat maple/installer/MakeBook.mpl - \
	     | $(MAPLE) -q

book-install: $(book)
	echo '(PackageTools:-Install)("$(book)",overwrite);' \
	      | $(MAPLE) -q

book-uninstall:
	echo '(PackageTools:-Uninstall)("$(book)");' \
	      | $(MAPLE) -q

# }}}
# {{{ Install/uninstall

help: $(call print-separator)

MKDIR = if test ! -d $(1); then mkdir --parents $(1); fi

install: $(call print-help,install,	Install everything)
install: $(addsuffix -install,info lisp mla)

uninstall: $(call print-help,uninstall,Remove installed files)
uninstall: lisp-uninstall
	$(RM) $(addprefix $(INFO-DIR)/,$(PKG).info)
	$(RM) $(MAPLE-LIB-DIR)/*

.PHONY: install uninstall



# }}}
# {{{ Installer

help: $(call print-separator)

.PHONY: installer installer-zip

CreateInstaller := maple/installer/CreateInstaller.mpl

installer := $(PKG)-installer-$(VERSION).mla

installer: $(call print-help,installer,Create Maple installer: $(installer))
installer: $(installer)

$(installer): $(CreateInstaller) mla info lisp/maplev-release.el $(addprefix doc/maplev,.pdf .info .html)
	@[ "$$(git rev-parse --abbrev-ref HEAD)" = release ] || echo $(call warn,"Not on release branch")
	@$(call shellerr, $(MAPLE) -q $<)


installer-zip := $(PKG)-ins-$(subst .,-,$(VERSION)).zip
installer-zip: $(call print-help,installer-zip,Create Maple installer zip file: $(installer-zip))
installer-zip: $(installer-zip)

$(installer-zip): $(installer) README-installer run-installer run-installer.bat
	zip $@ $^

# }}}

# {{{ Release

help: $(call print-separator)

.PHONY: check-branch check-clean check-release release

RELEASE-REGEX := \([0-9]\+\.\)\+[0-9]\+

release: $(call print-help,release,	Create maple release; use RELEASE=VALUE)
release: check-release check-branch check-clean
	@git checkout release
	@git merge --no-ff --message="merge branch develop into release" develop
	@sed --in-place "/^\*\*\Version /s/$(RELEASE-REGEX)/$(RELEASE)/" README.md
	@sed --in-place "/VERSION=/s/$(RELEASE-REGEX)/$(RELEASE)/" run-installer
	@doc/MakeVersion $(TEXI-VERSION) $(RELEASE)
	@lisp/MakeVersion $(LISP-VERSION) $(RELEASE)
	@git commit --quiet --message="prepare release" --all
	@git tag "release-$(RELEASE)"
	@git checkout master
	@git merge --message="merge branch 'release'" release
	@git checkout develop
	@git merge --message="merge branch 'master' into $branch" master
	@lisp/MakeVersion $(LISP-RELEASE) $(RELEASE)

check-clean:
	@git update-index -q --refresh
	@git diff-files --quiet -- \
	    || ( echo $(call warn,"unstaged changes") && exit 1 )
	@git diff-index --cached --quiet HEAD -- \
	    || ( echo $(call warn,"index contains uncommitted changes") && exit 1 )

check-release:
	@[[ "$(RELEASE)" =~ ^([0-9]+\.)+[0-9]+$$ ]] \
	    || ( echo $(call warn,"invalid or missing RELEASE value") && exit 1 )

check-branch:
ifneq ($(GIT-BRANCH),develop)
	$(error "must be on develop branch")
endif


maplev-built.zip: maplev.mla doc/maplev.html doc/maplev.info doc/maplev.pdf
	zip $@ $^


# }}}
# {{{ Distribution

help: $(call print-separator)

DIST-extra = Copyright README.md RELEASE-NOTES maplev.mla doc/maplev.info
DIST-FILES-extra = ChangeLog Makefile /usr/local/include/help-system.mak

dist: $(call print-help,dist,	Create $(PKG)-$$TAG.tar.gz file)
dist: $(LISP-VERSION) $(EL-FILES) $(MAPLE-FILES) $(TEXI-FILES)
	$(RM) -r $(PKG)-$(VERSION)
	$(call MKDIR,$(PKG)-$(VERSION))
	$(call MKDIR,$(PKG)-$(VERSION)/doc)
	$(call MKDIR,$(PKG)-$(VERSION)/lisp)
	$(call MKDIR,$(PKG)-$(VERSION)/maple)
	$(CP) $(EL-FILES) $(PKG)-$(VERSION)/lisp
	$(CP) $(MAPLE-FILES) $(PKG)-$(VERSION)/maple
	$(CP) $(TEXI-FILES) $(PKG)-$(VERSION)/doc
	$(CP) $(DIST-FILES-extra) $(DIST-extra) $(PKG)-$(VERSION)/
	zip -r $(PKG)-$(VERSION).zip $(PKG)-$(VERSION)
	tar zcvf $(PKG)-$(VERSION).tar.gz $(PKG)-$(VERSION)

.PHONY:  dist

# }}}
# {{{ P4

p4dir = /home/joe/maplesoft/sandbox/groups/scripts/share/emacs/$(PKG)
p4put: 
	$(CP) $(EL-FILES) $(p4dir)/lisp
	$(CP) $(MAPLE-FILES) $(p4dir)/maple
	$(CP) $(TEXI-FILES) $(p4dir)/doc
	$(CP) $(DIST-extra) $(DIST-FILES-extra) $(p4dir)

p4get: 
	$(CP) $(pfdir)/. .

.PHONY: p4put p4get

# }}}
# {{{ Clean

help: $(call print-separator)

clean: $(call print-help,clean,	Remove created and aux files)
clean: lisp-clean doc-clean mla-clean

.PHONY: clean

# }}}
