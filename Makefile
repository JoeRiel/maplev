# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <joer@san.rr.com>

PKG := maplev

VERSION := 3.2.2

# Activate selected make sections

MLA-DEPENDS := $(wildcard maple/Install/*)
LINEINFO_RELPATH := true

PKG-EXTRA := README
LISP-DIR := $(HOME)/.emacs.d/elpa/maplev-$(VERSION)

MPL-PKG ?= $(PKG)
ELISP-PKG ?= $(PKG)

SHELL := /bin/bash

# {{{ help

help:
	@echo $(if $(need-help),,Type \'$(MAKE)$(dash-f) help\' to get help)

need-help := $(filter help,$(MAKECMDGOALS))

define print-help
$(if $(need-help),$(info $1	$2))
endef

define print-separator
$(if $(need-help),$(info ----------------------------------------------------------------))
endef

define last-element
$(lastword $1)
endef

this-makefile   := $(call last-element,$(MAKEFILE_LIST))
other-makefiles := $(filter-out $(this-makefile),$(MAKEFILE_LIST))
parent-makefile := $(call last-element,$(other-makefiles))

dash-f := $(if $(filter-out Makefile makefile GNUmakefile,\
$(parent-makefile)), -f $(parent-makefile))

.PHONY: help
# }}}
# {{{ aux-funcs

txtbold   := $(shell tput bold)
# 0=black 1=red 2=green 3=yellow 4=blue 5=magenta 6=cyan 7=white
txthilite := $(shell tput setaf 3)
txtnormal := $(shell tput sgr0)
warn = "$(txthilite)$1$(txtnormal)"
shellerr = $(call showerr,$1 2>&1 > /dev/null)
showerr = err="$$($1)" ; if [ "$$err" ]; then echo $(call warn,$$err); fi

# }}}

comma := ,
OS := $(shell uname -o)

.PHONY: version
VERSION-REGEX := \([0-9]\+\.\)\+[0-9]\+

# {{{ Executables

# Assign local variables for needed binaries

BROWSER := x-www-browser
CP    := cp --archive --verbose
EMACS ?= emacs
HAVE-MLOAD := $(shell command -v mload 2> /dev/null)
INFO  := info
INFOVIEWER := info
MAKEINFO ?= makeinfo
MAPLE ?= maple
MHELP := mhelp  # custom script
MINT  := mint
MKDIR := mkdir --parents
MTAGS := mtags
PDFVIEWER := xpdf
TEXI2HTML := $(MAKEINFO) --html --number-sections
# TEXI2PDF := texi2pdf
TEXI2PDF := $(MAKEINFO) --pdf

# }}}
# {{{ Directories and Files

# install directories

INFO-DIR ?= $(HOME)/share/info
LISP-DIR ?= $(HOME)/.emacs.d/elpa
TBOX-DIR := $(HOME)/maple/toolbox/$(PKG)

# where the maple archive and help database go
MAPLE-DATA-DIR    := $(TBOX-DIR)/data
MAPLE-LIB-DIR     := $(TBOX-DIR)/lib
MAPLE-INSTALL-DIR := $(MAPLE-LIB-DIR)

# File used by mpldoc to build indices
MPLDOC-INDEX := maple/mpldoc-index

# }}}

# Maple

# {{{ mla

help: $(call print-separator)

.PHONY: mla mla-install mla-version

mms ?= $(wildcard maple/src/*.mm maple/include/*)
mds ?= $(wildcard maple/src/*.md)

VERSION-MI := maple/include/version.mi

mla-version: $(VERSION-MI)

help: $(call print-help,mla-version,Update $(VERSION-MI))

$(VERSION-MI): maple/src/$(MPL-PKG).mpl $(filter-out $(VERSION-MI),$(mms)) maple/include/About.mi
	@echo "Updating $@" ; \
	 echo '$$define __GIT_HASH__' "\"$$(git rev-parse HEAD)\"" > $@ ; \
	 echo '$$define __GIT_BUILD__' "\"$$(git describe --match=release\* 2> /dev/null)\"" >> $@ ; \
	 echo '$$define __VERSION__  "$(VERSION)"'      >> $@


mla := $(MPL-PKG).mla
help: $(call print-help,mla	,Create Maple archive: $(mla))
mla: $(mla)

mla-installed := $(TBOX-DIR)/lib/$(mla)

# Build mla.  This assumes the %.mpl file
# contains the line #savelib('pkg');
# Does it?  That isn't always the case.

%.mla: maple/src/%.mpl $(mms) $(version) $(MLA-DEPENDS)
	@$(RM) $@
	@echo "Building Maple archive $@"
ifneq ($(HAVE-MLOAD),)
	mload --quiet --lineinfo --reindex --readonly \
	--log=${MPL-PKG}.log \
	$(if ${LINEINFO_RELPATH},--relpath,--include=$(CURDIR)) \
	--mla=$@ $<
else
	@printf '%s\n' 'read "$<":' \
	'LibraryTools:-Create("$@"):' \
	'LibraryTools:-Save(`$*`, "$@"):' \
	| $(MAPLE) -q -B -I $(CURDIR) > ${MPL-PKG}.log 2>&1
	@if grep -q 'rror' ${MPL-PKG}.log || [ ! -s $@ ]; then cat ${MPL-PKG}.log; $(RM) $@; exit 1; fi
endif

help: $(call print-help,mla-install,Install mla into $(MAPLE-LIB-DIR))
mla-install: $(mla-installed)

$(mla-installed): $(mla)
	@$(MKDIR) $(MAPLE-LIB-DIR)
	@$(CP) $+ $@

# }}}
# {{{ hlp

# Install the help file.

help: $(call print-separator)

.PHONY: hlp hlp-install hlp-remove

hlp := maple/$(MPL-PKG).help
hlp-installed := $(TBOX-DIR)/lib/$(hlp)

help: $(call print-help,hlp-install,Install $(hlp) in $(MAPLE-INSTALL-DIR))
hlp-install: $(hlp)
	@$(MKDIR) $(MAPLE-INSTALL-DIR)
	@$(CP) $+ $(hlp-installed)

help: $(call print-help,hlp-remove,Remove $(MAPLE-INSTALL-DIR)/$(hlp))
hlp-remove:
	$(RM) --verbose $(MAPLE-INSTALL-DIR)/$(hlp)

# }}}

# {{{ data

ifeq ("$(DATA)","true")

help: $(call print-separator)

.PHONY: data-install

help: $(call print-help,data-install,Install $$data into $(MAPLE-DATA-DIR))
help: $(call print-help,data-links,Install links to the data files)
help: $(call print-help,data-remove,Remove data files and links)

ifneq ("$(data)","")
data-install: $(data)
	@$(MKDIR) $(MAPLE-DATA-DIR)
	@echo "Installing data files into $(MAPLE-DATA-DIR)/"
	@$(CP) $^ $(MAPLE-DATA-DIR)

data-links: $(data)
	@$(MKDIR) $(MAPLE-DATA-DIR)
	@echo "Installing data files into $(MAPLE-DATA-DIR)/"
	@ln --no-dereference --force --symbolic --target-directory=$(MAPLE-DATA-DIR) $(realpath $^)
endif

data-remove:
	@rm --recursive --dir $(MAPLE-DATA-DIR)

endif

# }}}

# {{{ tags

.PHONY: tags

help: $(call print-separator)
help: $(call print-help,tags	,Create TAGS file)
tags:
	$(MTAGS) maple/src/*

# }}}
# {{{ mint

help: $(call print-separator)

help: $(call print-help,mint	,Check Maple syntax)
mint:
	@$(call showerr,$(MINT) -q -i2 -D MINTONLY -I maple < maple/src/$(MPL-PKG).mpl)

# }}}
# {{{ test

ifeq ("$(TEST)","true")

help: $(call print-separator)

.PHONY: test test-clean test-extract test-run

# TESTER := tester -maple "smaple -B -I $(dir $(PWD))"
TESTER := tester -maple "smaple -B -I $(PWD)/maple"

help: $(call print-help,test	,Extract and run test suite)
help: $(call print-help,test-clean,Delete extracted tests)
help: $(call print-help,test-extract,Extract test suite)
help: $(call print-help,test-run,Run test suite)

test: test-extract test-run


test-run:
	@echo Running tests
	@$(RM) *.fail
	@err=$$(ls maple/mtest/*.tst 2> /dev/null | xargs -P4 -n1 $(TESTER) | sed -n '/Maple failed these tests/{n;n;p}') ; \
		if [ "$$err" ]; then echo "Maple failed these tests:" ; $(call showerr,echo "$$err"); fi
	@if [ -d maple/tst ]; then \
		$(call showerr,$(TESTER) maple/tst/*.tst 2>&1 | egrep --after-context=4 'Warning|Error') ; \
	fi

test-clean:
	@$(RM) maple/mtest/*

test-extract:
	@echo Extracting tests
	@$(RM) maple/src/_preview_.mm
	@$(RM) maple/mtest/*
	@$(call showerr,mpldoc --config nightly maple/src 2>&1 | egrep --after-context=2 'Warning|Error')

endif

# }}}

# Emacs

# {{{ elisp

help: $(call print-separator)

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(progn \
			(add-to-list (quote load-path) (expand-file-name \"./lisp\")) \
			(add-to-list (quote load-path) \"$(LISP-DIR)\") \
			(add-to-list (quote load-path) \
                                     (file-name-concat \"$(HOME)\" \
					\".emacs.d/elpa\" \"$(PKG)-$(VERSION)\")))"

# (add-to-list (quote load-path) (expand-file-name \".emacs.d/el-get/find-file-in-project\" \"$(HOME)\")))"

ELC = $(EMACS) --batch $(ELFLAGS) $(EXTRA_ELFLAGS) --funcall=batch-byte-compile

EXTRA_ELFLAGS ?=

#LISP-VERSION := lisp/$(ELISP-PKG)-version.el
#LISP-RELEASE := lisp/$(ELISP-PKG)-release.el
EL-FILES = $(wildcard lisp/*.el) $(LISP-VERSION) $(LISP-RELEASE)
# EL-FILES-NO-VERSION = $(filter-out $(LISP-VERSION),$(EL-FILES))
# LISP-FILES = $(ELS:%=lisp/%.el)

# convert EL-FILES and remove $(PKG)-pkg.el, which uses define-package
ELC-FILES = $(filter-out lisp/$(PKG)-pkg.elc,$(EL-FILES:.el=.elc))

#$(LISP-RELEASE): $(filter-out $(LISP-RELEASE) $(LISP-VERSION),$(EL-FILES))
#	@lisp/MakeVersion $@ $(VERSION)

%.elc : %.el
	@$(RM) $@
	@echo Byte-compiling $^
	@$(call showerr,$(ELC) $< 2>&1 > /dev/null | sed '/^Wrote/d')

version::
	sed --quiet '/^;; Version:/s/${VERSION-REGEX}/${VERSION}/p' lisp/${PKG}.el
	[ -f lisp/${PKG}-pkg.el ] && sed --quiet '/${PKG}/s/${VERSION-REGEX}/${VERSION}/p' lisp/${PKG}-pkg.el

help: $(call print-help,byte-compile,Byte-compile $$(EL-FILES))
byte-compile: $(EL-FILES) $(ELC-FILES)

help: $(call print-help,lisp-clean,Remove byte-compiled files)
lisp-clean:
	$(RM) $(ELC-FILES)

help: $(call print-help,lisp-install,Install lisp in $(LISP-DIR))
lisp-install: $(LISP-FILES)
	$(MKDIR) $(LISP-DIR)
	$(EMACS) --batch $(ELFLAGS) --eval="(package-install \"lisp/$(PKG).el\")"

help: $(call print-help,lisp-uninstall,Remove installed lisp files)
lisp-uninstall:
	@echo "removing installed lisp files"
	@$(RM) $(addprefix $(LISP-DIR)/,$(notdir $(EL-FILES) $(ELC-FILES)))

help: $(call print-help,links-install,Install links to the lisp files)
links-install: $(EL-FILES) $(ELC-FILES)
	@$(MKDIR) $(LISP-DIR)
	@ln -nfst $(LISP-DIR) $(realpath $^)

help: $(call print-help,links-clean,Remove links to the byte-compiled lisp files)
links-clean:
	$(RM) $(addprefix $(LISP-DIR)/,$(notdir $(ELC-FILES)))

help: $(call print-help,lisp-test,Test the lisp files)

lisp-test:
	@cd lisp/test && ./testall

.PHONY: byte-compile lisp-clean links-install lisp-install lisp-test lisp-uninstall

# }}}
# {{{ info
help: $(call print-separator)

TEXI-VERSION = doc/version.texi

help: $(call print-help,texi-version,Update $(TEXI-VERSION))
texi-version: $(TEXI-VERSION)

$(TEXI-VERSION):
	@echo @comment $@ -- auto-generated file, do not edit. > $@
	@echo @set VERSION $(VERSION) >> $@
	@echo @set DATE $$(date "+$d %B %Y") >> $@
	@echo @comment $@ ends here. >> $@

INFO-FILE  = doc/$(ELISP-PKG).info
PDF-FILE   = doc/$(ELISP-PKG).pdf
TEXI-FILES = doc/$(ELISP-PKG).texi $(TEXI-VERSION)
HTML-FILE  = doc/$(ELISP-PKG).html

DOC-FILES = $(TEXI-FILES) $(INFO-FILE) $(PDF-FILE) $(HTML-FILE)

help: $(call print-help,doc,	Create the info and html documentation)
doc:  info html
help: $(call print-help,info,	Create info file)
info: doc/$(ELISP-PKG).info
help:  $(call print-help,pdf,	Create pdf documentation)
pdf:  doc/$(ELISP-PKG).pdf
help:  $(call print-help,html,	Create html documentation)
html: doc/$(ELISP-PKG).html


doc/$(ELISP-PKG).pdf: doc/$(ELISP-PKG).texi $(TEXI-VERSION)
	(cd doc; $(TEXI2PDF) $(ELISP-PKG).texi)

doc/$(ELISP-PKG).info: doc/$(ELISP-PKG).texi $(TEXI-VERSION)
	(cd doc; $(MAKEINFO) \
	  --no-split $(ELISP-PKG).texi \
	  --output=$(ELISP-PKG).info)

doc/$(ELISP-PKG).html: doc/$(ELISP-PKG).texi $(TEXI-VERSION)
	(cd doc; $(TEXI2HTML) --no-split -o $(ELISP-PKG).html $(ELISP-PKG).texi)

help: $(call print-help,doc-clean,Remove the auxiliary files in doc)
doc-clean:
	$(RM) $(filter-out $(TEXI-FILES) $(DOC-FILES) $(INFO-FILE) doc/MakeVersion doc/fdl.texi, $(wildcard doc/*))

help: $(call print-help,doc-clean-all,Remove all generated documentation)
doc-clean-all: doc-clean
	$(RM) $(INFO-FILE) $(PDF-FILE) $(HTML-FILE)

help: $(call print-help,info-install,Install info files in $(INFO-DIR))
info-install: $(INFO-FILE)
	@$(MKDIR) $(INFO-DIR)
	$(CP) $(INFO-FILE) $(INFO-DIR)
	@echo Be sure to update 'dir' node
	@for file in $(INFO-FILE); do ginstall-info --debug --info-dir=$(INFO-DIR) $${file}; done

.PHONY: doc html info pdf doc-clean doc-clean-all p i h info-install texi-version $(TEXI-VERSION)

# preview pdf
help: $(call print-help,p,	Preview the pdf)
p: doc/$(ELISP-PKG).pdf
	$(PDFVIEWER) $<

# preview info
help: $(call print-help,i,	Preview the info)
i: doc/$(ELISP-PKG).info
	$(INFOVIEWER) $<

# preview html
help: $(call print-help,h,	Preview the html)
h: doc/$(ELISP-PKG).html
	$(BROWSER) $<

# }}}
# {{{ package

help: $(call print-separator)

# Create emacs package (tar file with el files, info files, and dir file;
# see doc for package.el.

.PHONY: package package-delete package-install package-uninstall

PKG-VER  := $(ELISP-PKG)-$(VERSION)
PKG-DIR  := /tmp/$(PKG-VER)
TAR-FILE := $(PKG-VER).tar

help: $(call print-help,package,	Create emacs package (tar file))
package: $(PKG-DIR) $(TAR-FILE)

$(TAR-FILE): $(wildcard lisp/*.el) $(INFO-FILE) doc/dir $(PKG-EXTRA)
	@echo Create $@
	$(RM) -r $(PKG-DIR)
	mkdir $(PKG-DIR)
	$(CP) --target-directory=$(PKG-DIR) $^
	tar --create --verbose --file=$@ --directory=/tmp $(PKG-VER)

help: $(call print-help,package-delete,Delete the tar file)
package-delete:
	$(RM) $(TAR-FILE)

# copy files to $(PKG-DIR)
$(PKG-DIR): $(wildcard lisp/*.el) $(INFO-FILE) doc/dir $(PKG-EXTRA)
	@echo Copy to $@
	$(RM) -r $@
	mkdir $@
	$(CP) --target-directory=$@ $^

help: $(call print-help,package-install,Install the tar file)
package-install: $(TAR-FILE)
	$(EMACS) --batch $(ELFLAGS) --eval="(package-install-file \"$<\")"

help: $(call print-help,package-uninstall,Uninstall the package)
package-uninstall:
	$(RM) --recursive ~/.emacs.d/elpa/$(PKG-VER)

BOOK-FILES += $(TAR-FILE)

# }}}

# {{{ install

help: $(call print-separator)

install-all := $(addsuffix -install,hlp mla data)

.PHONY: install $(install-all) uninstall

install-all: $(install-all)

help: $(call print-help,install	,Install everything)
install: install-all


help: $(call print-help,uninstall,Remove directory $(TBOX-DIR))
uninstall:
	@rm -rf $(TBOX-DIR)

# }}}
# {{{ clean

help: $(call print-separator)

.PHONY: clean cleanall sweep

help: $(call print-help,sweep	,Remove editor temp files)
sweep:
	@find . -wholename './.git' -prune -o -name '*~' -print -exec rm {} \+

help: $(call print-help,clean	,Remove built files and sweep)
clean: sweep
	-$(RM) maple/src/_preview_.mm maple/mdoc/* maple/mhelp/* maple/mtest/* maple/doti/* maple/src/*.{mtest,tst,mw} *.fail
	-$(RM) $(mla) $(hlp) $(book)

help: $(call print-help,cleanall,Remove intermediate files)
cleanall: clean
	-$(RM) -r maple/mdoc maple/mhelp maple/mtest

# }}}

# {{{ prebuilt

help: $(call print-separator)

.PHONY: prebuilt

prebuilt-zip := $(PKG)-built.zip

help: $(call print-help,prebuilt,Build $(prebuilt-zip))

prebuilt: $(prebuilt-zip)

doc-files := $(addprefix doc/$(PKG).,html info pdf) $(TEXI-VERSION)

$(prebuilt-zip):
	$(RM) $@
	zip $@ $(doc-files)
	[ -d lib ] || mkdir lib
	cp $(PKG).mla $(PKG).help lib
	zip $@ lib/*
	cd pmaple ; zip ../$@ bin*/*

help: $(call print-help,prebuilt-upload,Upload $(prebuilt-zip))
prebuilt-upload:
	gh release create "release-$(VERSION)" $(prebuilt-zip) --title "Release $(VERSION)"

help: $(call print-help,prebuilt-download,Download $(prebuilt-zip))
prebuilt-download:
	gh release download --clobber --repo JoeRiel/maplev "release-$(VERSION)"

help: $(call print-help,prebuilt-unpack,Unpack $(prebuilt-zip))
prebuilt-unpack:
	unzip -o $(prebuilt-zip)


# }}}

# makefile ends here
